{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Interpreter (interpret) where

import Control.Monad (foldM)
import Data.Char (isDigit)
import Dictionary (Dictionary (..), InterpreterError (..), OpResult, Operand (..), closureFromDs, dictStackLookup)

-- | Tokenize a string by splitting on whitespace (space, newline, tab).
--
-- If a parenthesis is opened, it will be treated as a single token until the closing parenthesis.
-- If an open parenthesis is not closed, an error will be returned.
-- If a closing parenthesis is found without an open parenthesis, an error will be returned.
--
-- If a bracket is opened, it will be treated as a single token until the closing bracket.
-- If an open bracket is not closed, an error will be returned.
-- If a closing bracket is found without an open bracket, an error will be returned.
--
-- Note:
-- Interestingly, when playing with the Ghostscript compiler, I found that "(())" => ["(())"] but "(()" => StringNeverClosed and "())" => StringNeverOpened.
-- So, you can do "nested" strings, but parentheses must be balanced.
-- Further, brackets can be placed inside of parentheses ignoring balance. (e.g. "({})" => ["({})"])
tokenize :: String -> Either InterpreterError [String]
tokenize input = go input "" [] 0 0
  where
    -- Recursive function
    go :: String -> String -> [String] -> Int -> Int -> Either InterpreterError [String]
    go [] current acc 0 0
      | null current = Right (reverse acc)
      | otherwise = Right (reverse (current : acc))
    go [] _ _ pDepth bDepth
      | pDepth > 0 = Left StringNeverClosed
      | bDepth > 0 = Left ProcNeverClosed
    go [] _ _ _ _ = Left StringNeverClosed -- Defensive catch-all
    go (c : cs) current acc pDepth bDepth = case c of
      '(' | bDepth == 0 -> handleOpenParen cs current acc pDepth bDepth
      ')' | bDepth == 0 -> handleCloseParen cs current acc pDepth bDepth
      '{' | pDepth == 0 -> handleOpenBracket cs current acc pDepth bDepth
      '}' | pDepth == 0 -> handleCloseBracket cs current acc pDepth bDepth
      _ | isWhitespace c && pDepth == 0 && bDepth == 0 -> handleWhitespace cs current acc pDepth bDepth
      _ -> go cs (current ++ [c]) acc pDepth bDepth

    handleOpenParen cs current acc pDepth bDepth
      | null current && pDepth == 0 = go cs "(" acc (pDepth + 1) bDepth
      | otherwise = go cs (current ++ "(") acc (pDepth + 1) bDepth

    handleCloseParen cs current acc pDepth bDepth
      | pDepth == 0 = Left StringNeverOpened
      | pDepth == 1 = go cs [] ((current ++ ")") : acc) 0 bDepth
      | otherwise = go cs (current ++ ")") acc (pDepth - 1) bDepth

    handleOpenBracket cs current acc pDepth bDepth
      | null current && bDepth == 0 = go cs "{" acc pDepth (bDepth + 1)
      | otherwise = go cs (current ++ "{") acc pDepth (bDepth + 1)

    handleCloseBracket cs current acc pDepth bDepth
      | bDepth == 0 = Left ProcNeverOpened
      | bDepth == 1 = go cs [] ((current ++ "}") : acc) pDepth 0
      | otherwise = go cs (current ++ "}") acc pDepth (bDepth - 1)

    handleWhitespace cs current acc pDepth bDepth
      | null current = go cs "" acc pDepth bDepth
      | otherwise = go cs "" (current : acc) pDepth bDepth

    isWhitespace c = c `elem` [' ', '\n', '\t']

stripProc :: String -> String
stripProc = init . tail

handleProc :: [Dictionary] -> [Operand] -> String -> Either InterpreterError OpResult
#ifndef USE_STATIC_SCOPING
-- Dynamic scoping, pass the dictionary stack
handleProc ds os p  = interpret ds os (stripProc p)
#else
-- Static scoping, pass a closure of the current dictionary stack, then return the unchanged dictionary stack
handleProc ds os p = do
  (_, os') <- interpret [closureFromDs ds] os (stripProc p)
  return (ds, os')
#endif

-- | Given a global dictionary and code, interprets the code and returns stacks or an error.
--
-- Code is first tokenized. In this lexical analysis process, strings and procedures are identified and grouped.
--
-- Then, the tokens are processed one by one. There is no need to build an AST because the PostScript language is stack-based.
-- Because of its stack-based nature, semantic analysis is done before syntax analysis:
--
-- 1. Determine the symbol type. If undefined, return an error. (semantic analysis)
-- 2. If the symbol is a operand, push it onto the operand stack.
-- 3. If the symbol is an operator, perform syntax analysis. If the wrong type operands are on the stack, return an error. (syntax analysis)
-- 4. Perform the operation and modify the operand stack and dictionary stack accordingly. The operator may recursively call the interpreter.
--
-- Returns an InterpreterError on failure.
interpret :: [Dictionary] -> [Operand] -> String -> Either InterpreterError OpResult
interpret ds os code = do
  tokens <- tokenize code
  foldM processToken (ds, os) tokens
  where
    processToken :: OpResult -> String -> Either InterpreterError OpResult
    processToken (ds', os') token
      | isInt token = Right (ds', OperandInt (read token) : os')
      | isDouble token = Right (ds', OperandDouble (read token) : os')
      | isStr token = Right (ds', OperandString token : os')
      | isBool token = Right (ds', OperandBool (token == "true") : os')
      | isName token = Right (ds', OperandName (tail token) : os')
      | isProc token = Right (ds', OperandProc token : os')
      | otherwise = findSymbol token ds' os'

    findSymbol :: String -> [Dictionary] -> [Operand] -> Either InterpreterError OpResult
    findSymbol token ds' os' = case dictStackLookup token ds' of
      Just op -> case op ds' os' of
        Right (ds'', OperandProc p : os'') -> handleProc ds'' os'' p
        Right (ds'', os'') -> Right (ds'', os'')
        Left err -> Left err
      Nothing -> Left (SymbolNotFound token)

    isStr, isBool, isName, isProc, isInt, isDouble :: String -> Bool
    isStr s = head s == '(' && last s == ')'
    isBool s = s == "true" || s == "false"
    isName s = head s == '/'
    isProc s = head s == '{' && last s == '}'
    isInt s = case s of
      ('-' : xs) -> all isDigit xs
      _ -> all isDigit s
    isDouble s = case reads s :: [(Double, String)] of
      [(_, "")] -> True
      _ -> False