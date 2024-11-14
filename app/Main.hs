module Main (main) where

import Data.Char
import GlobalDict

data InterpreterError = SymbolNotFound | OperandError OpError
  deriving (Show, Eq)

isPostscriptString :: String -> Bool
isPostscriptString s = head s == '(' && last s == ')'

lookupDictStackSymbol :: String -> [Dictionary] -> Maybe Operator
lookupDictStackSymbol _ [] = Nothing
lookupDictStackSymbol symbol (d : ds)  = case lookupDict symbol d of
  Just op -> Just op
  Nothing -> lookupDictStackSymbol symbol ds

-- | Given a global dictionary and code, interprets the code and returns stacks or an error.
-- Tokenize the code, then for each token:
-- - If the token is a number, push it to the operand stack.
-- - If the token is a string which starts with a ( and ends with a ), push it to the operand stack.
-- - If the token is a symbol, apply the symbol from the dictionary stack.
interpret :: [Dictionary] -> String -> Either InterpreterError OpResult
interpret ds code = foldl processToken (Right $ OpResult ds []) (words code)
  where
    processToken :: Either InterpreterError OpResult -> String -> Either InterpreterError OpResult
    processToken (Left err) _ = Left err -- Propogate error up
    processToken (Right (OpResult ds' os)) token
        | all isDigit token = Right $ OpResult ds' (OperandInt (read token) : os) -- Push number to operand stack as an OperandInt
        | isPostscriptString token = Right $ OpResult ds' (OperandString token : os) -- Push string to operand stack as an OperandString
        | otherwise = case lookupDictStackSymbol token ds' of
            Just op -> case op ds' os of
              Right (OpResult ds'' os') -> Right $ OpResult ds'' os'    -- Apply operator to dictionary and operand stack. Propogate new stacks up.
              Left err -> Left $ OperandError err                       -- Propogate operand error up
            Nothing -> Left SymbolNotFound                              -- Propogate symbol not found error up
            

main :: IO ()
main = do
  case interpret [globalDictionary] "1 2 add\n3 3 sub" of
    Right (OpResult _ os) -> print os
    Left err -> print err
