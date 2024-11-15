module Interpreter (interpret) where

import Data.Char (isDigit)
import Dictionary (Dictionary (..), InterpreterError (..), OpResult, Operand (..), dictStackLookup)

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
tokenize s = go s [] [] 0 0
  where
    -- \| Parameters:
    -- - current: the input string
    -- - acc: token accumulator (accumulated value until whitespace or delimiter)
    -- - tokens: stack of built tokens
    -- - pDepth: parenthesis depth
    -- - bDepth: bracket depth
    go :: String -> String -> [String] -> Int -> Int -> Either InterpreterError [String]
    go [] [] acc 0 0 = Right (reverse acc)
    go [] [] _ pDepth bDepth
      | pDepth > 0 = Left StringNeverClosed
      | bDepth > 0 = Left ProcNeverClosed
    go [] current acc 0 0 = Right (reverse (current : acc))
    go [] _ _ _ _ = Left StringNeverClosed
    go (c : cs) current acc pDepth bDepth
      | c == '(' && bDepth == 0 && pDepth == 0 = go cs "(" acc 1 bDepth
      | c == '(' && bDepth == 0 = go cs (current ++ "(") acc (pDepth + 1) bDepth
      | c == ')' && bDepth == 0 =
          if pDepth == 0
            then Left StringNeverOpened
            else
              let newPDepth = pDepth - 1
               in if newPDepth == 0
                    then go cs [] ((current ++ ")") : acc) 0 bDepth
                    else go cs (current ++ ")") acc newPDepth bDepth
      | c == '{' && pDepth == 0 && bDepth == 0 = go cs "{" acc pDepth 1
      | c == '{' && pDepth == 0 = go cs (current ++ "{") acc pDepth (bDepth + 1)
      | c == '}' && pDepth == 0 =
          if bDepth == 0
            then Left ProcNeverOpened
            else
              let newBDepth = bDepth - 1
               in if newBDepth == 0
                    then go cs [] ((current ++ "}") : acc) pDepth 0
                    else go cs (current ++ "}") acc pDepth newBDepth
      | c `elem` [' ', '\n', '\t'] && pDepth == 0 && bDepth == 0 =
          if null current
            then go cs [] acc pDepth bDepth
            else go cs [] (current : acc) pDepth bDepth
      | otherwise = go cs (current ++ [c]) acc pDepth bDepth

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
interpret ds os code = case tokenize code of
  Left err -> Left err
  Right tokens -> foldl processToken (Right (ds, os)) tokens
  where
    processToken :: Either InterpreterError OpResult -> String -> Either InterpreterError OpResult
    processToken (Left err) _ = Left err
    processToken (Right (ds', os')) token
      | isInt token = Right (ds', OperandInt (read token) : os')
      | isStr token = Right (ds', OperandString token : os')
      | isBool token = Right (ds', OperandBool (token == "true") : os')
      | isName token = Right (ds', OperandName (tail token) : os')
      | isProc token = Right (ds', OperandProc token : os')
      | otherwise = case dictStackLookup token ds' of
          Just op -> case op ds' os' of
            Right (ds'', OperandProc p : os'') -> interpret ds'' os'' (stripProc p) -- Recursively call the interpreter on procedure lookup
            Right (ds'', os'') -> Right (ds'', os'')
            Left err -> Left err
          Nothing -> Left (SymbolNotFound token)
    isStr, isBool, isName, isProc :: String -> Bool
    isStr s = head s == '(' && last s == ')'
    isBool s = s == "true" || s == "false"
    isName s = head s == '/'
    isProc s = head s == '{' && last s == '}'
    isInt = all isDigit

    stripProc :: String -> String
    stripProc = init . tail
