module GlobalDict (globalDictionary, lookupDict, Operand (..), Operator, Dictionary, OpError (..)) where

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap

data Operand = OperandInt Int | OperandBool Bool | OperandString String
  deriving (Show, Eq)

data OpError = TypeMismatchError String | StackUnderflowError
  deriving (Show, Eq)

type Operator = [Operand] -> Either OpError [Operand]

type Dictionary = HashMap String Operator

{--
required PostScript operators:

Stack Manipulation (e.g., exch, pop, copy, dup, clear, count)
Arithmetic Operations (e.g., add, sub, mul, div, mod, etc.)
Dictionary Operations (e.g., dict, length, begin, end, def)
String Operations (e.g., length, get, getinterval, putinterval)
Boolean Operations (e.g., eq, ne, gt, lt, and, or, not)
Flow Control (e.g., if, ifelse, for, repeat, quit)
Input/Output Operations (e.g., print, =, ==)

--}

{--#region Stack Manipulation--}

exch :: Operator
exch (x : y : stack) = Right $ y : x : stack
exch _ = Left StackUnderflowError

pop :: Operator
pop (_ : stack) = Right stack
pop _ = Left StackUnderflowError

copy :: Operator
copy (OperandInt n : stack)
  | n <= length stack = Right $ take n stack ++ stack
  | otherwise = Left StackUnderflowError
copy _ = Left StackUnderflowError

dup' :: Operator
dup' (x : stack) = Right $ x : x : stack
dup' _ = Left StackUnderflowError

clear :: Operator
clear _ = Right []

count :: Operator
count stack = Right $ OperandInt (length stack) : stack

stackOps :: [(String, Operator)]
stackOps = [("exch", exch), ("pop", pop), ("copy", copy), ("dup", dup'), ("clear", clear), ("count", count)]

{--#endregion Stack Manipulation--}

{--#region Arithmetic Operations--}

binaryIntOp :: (Int -> Int -> Int) -> Operator
binaryIntOp op (OperandInt x : OperandInt y : stack) = Right $ OperandInt (y `op` x) : stack
binaryIntOp _ (x : y : _) = Left $ TypeMismatchError $ "Expected two integers, but got: " ++ show x ++ " and " ++ show y
binaryIntOp _ _ = Left StackUnderflowError

arithmeticOps :: [(String, Operator)]
arithmeticOps = [("add", binaryIntOp (+)), ("sub", binaryIntOp (-)), ("mul", binaryIntOp (*)), ("div", binaryIntOp div), ("mod", binaryIntOp mod)]

{--#endregion Arithmetic Operations--}

-- | Lookup an operator in the dictionary
lookupDict :: String -> Dictionary -> Maybe Operator
lookupDict = HashMap.lookup

-- | Contains all PostScript operators
globalDictionary :: Dictionary
globalDictionary = HashMap.fromList (arithmeticOps ++ stackOps)