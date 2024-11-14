{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module GlobalDict where

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap

data Operand = OperandInt Int | OperandBool Bool | OperandString String
  deriving (Show, Eq)

data OpError = TypeMismatchError String | StackUnderflowError | DivisionByZeroError | IndexOutOfBoundsError
  deriving (Show, Eq)

type Operator = [Operand] -> Either OpError [Operand]

type Dictionary = HashMap String Operator

{--
required PostScript operators:

Dictionary Operations (e.g., dict, length, begin, end, def)
Flow Control (e.g., if, ifelse, for, repeat, quit)
Input/Output Operations (e.g., print, =, ==)

--}

{--#region Stack Manipulation--}

psExch :: Operator
psExch (x : y : stack) = Right $ y : x : stack
psExch _ = Left StackUnderflowError

psPop :: Operator
psPop (_ : stack) = Right stack
psPop _ = Left StackUnderflowError

psCopy :: Operator
psCopy (OperandInt n : stack)
  | n <= length stack = Right $ take n stack ++ stack
  | otherwise = Left StackUnderflowError
psCopy _ = Left StackUnderflowError

psDup :: Operator
psDup (x : stack) = Right $ x : x : stack
psDup _ = Left StackUnderflowError

psClear :: Operator
psClear _ = Right []

psCount :: Operator
psCount stack = Right $ OperandInt (length stack) : stack

stackOps :: [(String, Operator)]
stackOps = [("exch", psExch), ("pop", psPop), ("copy", psCopy), ("dup", psDup), ("clear", psClear), ("count", psCount)]

{--#endregion Stack Manipulation--}

{--#region Arithmetic Operations--}

binaryIntOp :: (Int -> Int -> Int) -> Operator
binaryIntOp op (OperandInt x : OperandInt y : stack) = Right $ OperandInt (y `op` x) : stack
binaryIntOp _ (x : y : _) = Left $ TypeMismatchError $ "Expected two integers, but got: " ++ show x ++ " and " ++ show y
binaryIntOp _ _ = Left StackUnderflowError

psAdd :: Operator
psAdd = binaryIntOp (+)

psSub :: Operator
psSub = binaryIntOp (-)

psMul :: Operator
psMul = binaryIntOp (*)

psDiv :: Operator
psDiv (OperandInt 0 : _) = Left DivisionByZeroError
psDiv operands = binaryIntOp div operands

psMod :: Operator
psMod = binaryIntOp mod

arithmeticOps :: [(String, Operator)]
arithmeticOps = [("add", psAdd), ("sub", psSub), ("mul", psMul), ("div", psDiv), ("mod", psMod)]

{--#endregion Arithmetic Operations--}

{--#region String Operations--}

psLength :: Operator
psLength (OperandString s : stack) = Right $ OperandInt (length s) : stack
psLength (_ : _) = Left $ TypeMismatchError "Expected a string at the top of the stack"
psLength [] = Left StackUnderflowError

psGet :: Operator
psGet (OperandInt i : OperandString s : stack)
  | i >= 0 && i < length s = Right $ OperandInt (fromEnum (s !! i)) : stack
  | otherwise = Left IndexOutOfBoundsError
psGet (_ : _ : _) = Left $ TypeMismatchError "Expected an integer and a string at the top of the stack"
psGet _ = Left StackUnderflowError

psGetInterval :: Operator
psGetInterval (OperandInt i : OperandInt n : OperandString s : stack)
  | i >= 0 && i + n <= length s = Right $ OperandString (take n (drop i s)) : stack
  | otherwise = Left IndexOutOfBoundsError
psGetInterval (_ : _ : _ : _) = Left $ TypeMismatchError "Expected two integers and a string at the top of the stack"
psGetInterval _ = Left StackUnderflowError

psPutInterval :: Operator
psPutInterval (OperandString replacement : OperandInt si : OperandString s : stack)
  | si >= 0 && si + length replacement <= length s = Right $ OperandString (take si s ++ replacement ++ drop (si + length replacement) s) : stack
  | otherwise = Left IndexOutOfBoundsError
psPutInterval (_ : _ : _ : _) = Left $ TypeMismatchError "Expected two strings and an integer at the top of the stack"
psPutInterval _ = Left StackUnderflowError

stringOps :: [(String, Operator)]
stringOps = [("length", psLength), ("get", psGet), ("getinterval", psGetInterval), ("putinterval", psPutInterval)]

{--#endregion String Operations--}

{--#region Boolean Operations--}

psEq :: Operator
psEq (x : y : stack) = Right $ OperandBool (x == y) : stack
psEq _ = Left StackUnderflowError

psNe :: Operator
psNe (x : y : stack) = Right $ OperandBool (x /= y) : stack
psNe _ = Left StackUnderflowError

psGt :: Operator
psGt (OperandInt x : OperandInt y : stack) = Right $ OperandBool (y > x) : stack
psGt (x : y : _) = Left $ TypeMismatchError $ "Expected two integers, but got: " ++ show x ++ " and " ++ show y
psGt _ = Left StackUnderflowError

psLt :: Operator
psLt (OperandInt x : OperandInt y : stack) = Right $ OperandBool (y < x) : stack
psLt (x : y : _) = Left $ TypeMismatchError $ "Expected two integers, but got: " ++ show x ++ " and " ++ show y
psLt _ = Left StackUnderflowError

binaryBoolOp :: (Bool -> Bool -> Bool) -> Operator
binaryBoolOp op (OperandBool x : OperandBool y : stack) = Right $ OperandBool (y `op` x) : stack
binaryBoolOp _ (x : y : _) = Left $ TypeMismatchError $ "Expected two booleans, but got: " ++ show x ++ " and " ++ show y
binaryBoolOp _ _ = Left StackUnderflowError

psAnd :: Operator
psAnd = binaryBoolOp (&&)

psOr :: Operator
psOr = binaryBoolOp (||)

psNot :: Operator
psNot (OperandBool x : stack) = Right $ OperandBool (not x) : stack
psNot (x : _) = Left $ TypeMismatchError $ "Expected a boolean, but got: " ++ show x
psNot _ = Left StackUnderflowError

booleanOps :: [(String, Operator)]
booleanOps = [("eq", psEq), ("ne", psNe), ("gt", psGt), ("lt", psLt), ("and", psAnd), ("or", psOr), ("not", psNot)]

{--#endregion Boolean Operations--}

-- | Lookup an operator in the dictionary
lookupDict :: String -> Dictionary -> Maybe Operator
lookupDict = HashMap.lookup

-- | Contains all PostScript operators
globalDictionary :: Dictionary
globalDictionary = HashMap.fromList (arithmeticOps ++ stackOps ++ stringOps ++ booleanOps)