module Interpreter (interpretWithGlobalDict, InterpreterError (..), tokenize) where

import Data.Char
import GlobalDict

data InterpreterError = SymbolNotFound String | StringNeverClosed | StringNeverOpened | OperandError OpError
  deriving (Show, Eq)

isPostscriptString :: String -> Bool
isPostscriptString s = head s == '(' && last s == ')'

isPostscriptBool :: String -> Bool
isPostscriptBool s = s == "true" || s == "false"

isPostscriptName :: String -> Bool
isPostscriptName s = head s == '/'

lookupDictStackSymbol :: String -> [Dictionary] -> Maybe Operator
lookupDictStackSymbol _ [] = Nothing
lookupDictStackSymbol symbol (d : ds) = case lookupDict symbol d of
  Just op -> Just op
  Nothing -> lookupDictStackSymbol symbol ds

-- | Tokenize a string by splitting on whitespace (space, newline, tab).
-- If a parenthesis is opened, it will be treated as a single token until the closing parenthesis.
-- If an open parenthesis is not closed, an error will be returned.
-- If a closing parenthesis is found without an open parenthesis, an error will be returned.
--
-- Note:
-- Interestingly, when playing with the Ghostscript compiler, I found that "(())" => ["(())"] but "(()" => StringNeverClosed and "())" => StringNeverOpened.
-- So, you can do "nested" strings, but parentheses must be balanced.
--
-- Examples:
--
-- >>> tokenize "1 2 add"
-- Right ["1", "2", "add"]
--
-- >>> tokenize "1 (2 add) add"
-- Right ["1", "(2 add)", "add"]
--
-- >>> tokenize "("
-- Left StringNeverClosed
--
-- >>> tokenize ")"
-- Left StringNeverOpened
--
-- >>> tokenize "()"
-- Right ["()"]
--
-- >>> tokenize "(())"
-- Right ["(())"]
--
-- >>> tokenize "(()"
-- Left StringNeverClosed
--
-- >>> tokenize "())"
-- Left StringNeverOpened
tokenize :: String -> Either InterpreterError [String]
tokenize s = go s [] [] 0
  where
    go :: String -> String -> [String] -> Int -> Either InterpreterError [String]
    go [] [] acc 0 = Right (reverse acc) -- Successfully accumulated all tokens
    go [] [] _ _ = Left StringNeverClosed -- Open parenthesis not closed at end of input
    go [] current acc 0 = Right (reverse (current : acc)) -- Add last token if no parentheses are open
    go [] _ _ _ = Left StringNeverClosed -- If parentheses remain open at end of input
    go (c : cs) current acc depth
      | c == '(' && depth == 0 && not (null current) = go cs "(" (current : acc) 1
      | c == '(' = go cs (current ++ "(") acc (depth + 1)
      | c == ')' && depth == 0 = Left StringNeverOpened
      | c == ')' =
          let newDepth = depth - 1
           in if newDepth == 0
                then go cs [] ((current ++ ")") : acc) 0
                else go cs (current ++ ")") acc newDepth
      | c `elem` [' ', '\n', '\t'] && depth == 0 =
          if null current
            then go cs [] acc 0 -- Skip consecutive spaces, newlines, and tabs
            else go cs [] (current : acc) 0 -- Add accumulated token
      | otherwise = go cs (current ++ [c]) acc depth -- Add character to current token

-- | Given a global dictionary and code, interprets the code and returns stacks or an error.
-- Tokenize the code, then for each token:
-- - If the token is a number, push it to the operand stack.
-- - If the token is a string which starts with a ( and ends with a ), push it to the operand stack.
-- - If the token is a symbol, apply the symbol from the dictionary stack.
interpret :: [Dictionary] -> String -> Either InterpreterError OpResult
interpret ds code = case tokenize code of
  Left err -> Left err
  Right tokens -> foldl processToken (Right (ds, [])) tokens
  where
    processToken :: Either InterpreterError OpResult -> String -> Either InterpreterError OpResult
    processToken (Left err) _ = Left err -- Propogate error up
    processToken (Right (ds', os)) token
      | all isDigit token = Right (ds', OperandInt (read token) : os) -- Push number to operand stack as an OperandInt
      | isPostscriptString token = Right (ds', OperandString token : os) -- Push string to operand stack as an OperandString
      | isPostscriptBool token = Right (ds', OperandBool (token == "true") : os) -- Push boolean to operand stack as an OperandBool
      | isPostscriptName token = Right (ds, OperandName (tail token) : os) -- Push name to operand stack as an OperandName
      | otherwise = case lookupDictStackSymbol token ds' of
          Just op -> case op ds' os of
            Right (ds'', os') -> Right (ds'', os') -- Apply operator to dictionary and operand stack. Propogate new stacks up.
            Left err -> Left $ OperandError err -- Propogate operand error up
          Nothing -> Left (SymbolNotFound token) -- Propogate symbol not found error up

interpretWithGlobalDict :: String -> Either InterpreterError OpResult
interpretWithGlobalDict = interpret [globalDictionary]