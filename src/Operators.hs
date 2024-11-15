{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Operators where

import Data.Bits (Bits ((.&.), (.|.)), complement)
import qualified Data.HashMap.Strict as HashMap
import Dictionary
import Interpreter

{--#region Stack Manipulation--}
psExch :: Operator
psExch ds (x : y : os) = Right (ds, y : x : os)
psExch _ _ = Left StackUnderflowError

psPop :: Operator
psPop ds (_ : os) = Right (ds, os)
psPop _ _ = Left StackUnderflowError

psCopy :: Operator
psCopy ds (OperandInt n : os)
  | n <= length os = Right (ds, take n os ++ os)
  | otherwise = Left StackUnderflowError
psCopy _ _ = Left StackUnderflowError

psDup :: Operator
psDup ds (o : os) = Right (ds, o : o : os)
psDup _ _ = Left StackUnderflowError

psClear :: Operator
psClear ds _ = Right (ds, [])

psCount :: Operator
psCount ds os = Right (ds, OperandInt (length os) : os)

{--#endregion Stack Manipulation--}

{--#region Arithmetic Operations--}
binaryIntOp :: (Int -> Int -> Int) -> Operator
binaryIntOp op ds (OperandInt x : OperandInt y : os) = Right (ds, OperandInt (y `op` x) : os)
binaryIntOp _ _ (_ : _ : _) = Left TypeMismatchError
binaryIntOp _ _ _ = Left StackUnderflowError

psAdd, psSub, psMul, psMod :: Operator
psAdd = binaryIntOp (+)
psSub = binaryIntOp (-)
psMul = binaryIntOp (*)
psMod = binaryIntOp mod

psDiv :: Operator
psDiv _ (OperandInt 0 : _) = Left DivisionByZeroError
psDiv ds os = binaryIntOp div ds os

{--#endregion Arithmetic Operations--}

{--#region String Operations--}
pslen :: String -> Int
pslen = subtract 2 . length

wrapstr :: String -> String
wrapstr s = "(" ++ s ++ ")"

stripstr :: String -> String
stripstr = init . tail

psStrLength :: Operator
psStrLength ds (OperandString s : os) = Right (ds, OperandInt (pslen s) : os)
psStrLength _ (_ : _) = Left TypeMismatchError
psStrLength _ [] = Left StackUnderflowError

psGet :: Operator
psGet ds (OperandInt n : OperandString s : os)
  | n >= 0 && n < pslen s = Right (ds, OperandInt (fromEnum (s !! (n + 1))) : os)
  | otherwise = Left IndexOutOfBoundsError
psGet _ (_ : _ : _) = Left TypeMismatchError
psGet _ _ = Left StackUnderflowError

psGetInterval :: Operator
psGetInterval ds (OperandInt n : OperandInt count : OperandString s : os)
  | n >= 0 && n + count <= pslen s = Right (ds, OperandString (wrapstr $ take count (drop (n + 1) s)) : os)
  | otherwise = Left IndexOutOfBoundsError
psGetInterval _ (_ : _ : _ : _) = Left TypeMismatchError
psGetInterval _ _ = Left StackUnderflowError

psPutInterval :: Operator
psPutInterval ds (OperandString replacement : OperandInt si : OperandString s : os)
  | si >= 0 && si + pslen replacement <= pslen s = Right (ds, OperandString (wrapstr $ take si (stripstr s) ++ stripstr replacement ++ drop (si + pslen replacement) (stripstr s)) : os)
  | otherwise = Left IndexOutOfBoundsError
psPutInterval _ (_ : _ : _ : _) = Left TypeMismatchError
psPutInterval _ _ = Left StackUnderflowError

{--#endregion String Operations--}

{--#region Boolean Operations--}
psEq :: Operator
psEq ds (x : y : os) = Right (ds, OperandBool (x == y) : os)
psEq _ _ = Left StackUnderflowError

psNe :: Operator
psNe ds (x : y : os) = Right (ds, OperandBool (x /= y) : os)
psNe _ _ = Left StackUnderflowError

binaryBoolOp :: (Bool -> Bool -> Bool) -> (Int -> Int -> Int) -> Operator
binaryBoolOp binOp _ ds (OperandBool x : OperandBool y : os) = Right (ds, OperandBool (y `binOp` x) : os)
binaryBoolOp _ bitOp ds (OperandBool x : OperandInt y : os) = Right (ds, OperandInt (y `bitOp` fromEnum x) : os)
binaryBoolOp _ bitOp ds (OperandInt x : OperandBool y : os) = Right (ds, OperandInt (x `bitOp` fromEnum y) : os)
binaryBoolOp _ bitOp ds (OperandInt x : OperandInt y : os) = Right (ds, OperandInt (y `bitOp` x) : os)
binaryBoolOp _ _ _ (_ : _ : _) = Left TypeMismatchError
binaryBoolOp _ _ _ _ = Left StackUnderflowError

psAnd, psOr :: Operator
psAnd = binaryBoolOp (&&) (.&.)
psOr = binaryBoolOp (||) (.|.)

psNot :: Operator
psNot ds (OperandBool b : os) = Right (ds, OperandBool (not b) : os)
psNot ds (OperandInt b : os) = Right (ds, OperandInt (complement b) : os)
psNot _ (_ : _) = Left TypeMismatchError
psNot _ _ = Left StackUnderflowError

comparisonOp :: (Operand -> Operand -> Bool) -> Operator
comparisonOp op ds (x : y : os) = Right (ds, OperandBool (y `op` x) : os)
comparisonOp _ _ _ = Left StackUnderflowError

psGt, psLt :: Operator
psGt = comparisonOp (>)
psLt = comparisonOp (<)

{--#endregion Boolean Operations--}

{-- #region Dictionary Operations--}

psDict :: Operator
psDict ds (OperandInt n : os) = Right (ds, OperandDict (makeDict n) : os)
psDict _ _ = Left TypeMismatchError

psLengthDict :: Operator
psLengthDict ds (OperandDict (Dictionary _ d) : os) = Right (ds, OperandInt (HashMap.size d) : os)
psLengthDict _ _ = Left TypeMismatchError

psMaxlength :: Operator
psMaxlength ds (OperandDict (Dictionary n _) : os) = Right (ds, OperandInt n : os)
psMaxlength _ _ = Left TypeMismatchError

psBeginDict :: Operator
psBeginDict ds (OperandDict d : os) = Right (d : ds, os)
psBeginDict _ _ = Left TypeMismatchError

psEndDict :: Operator
psEndDict (_ : ds) os = Right (ds, os)
psEndDict _ _ = Left StackUnderflowError

-- | Creates an operator that pushes a value onto the operand stack
valueOperator :: Operand -> Operator
valueOperator o ds os = Right (ds, o : os)

psDef :: Operator
psDef (Dictionary n d : ds) (value : OperandName key : os)
  | HashMap.size d < n = Right (Dictionary n (HashMap.insert key (valueOperator value) d) : ds, os)
  | otherwise = Left StackOverflowError
psDef _ _ = Left TypeMismatchError

{--#endregion Dictionary Operations--}

-- {-- #region Procedure Operations--}
-- -- These operators will call the interpreter recursively to interpret the procedure.
-- -- Thus I am implementing them here to avoid circular dependencies.

-- psIf :: Operator
-- psIf ds (OperandBool b : OperandProc p : os) = if b then interpret ds os p else Right (ds, os)
-- psIf _ _ = Left TypeMismatchError

-- psIfElse :: Operator
-- psIfElse ds (OperandBool b : OperandProc p1 : OperandProc p2 : os) = if b then interpret ds os p1 else interpret ds os p2
-- psIfElse _ _ = Left TypeMismatchError

-- psFor :: Operator
-- psFor ds (OperandInt limit : OperandInt increment : OperandInt start : OperandProc p : os) = go start
--   where
--     go :: Int -> Either InterpreterError OpResult
--     go i
--       | i >= limit = Right (ds, os)
--       | otherwise = case interpret ds (OperandInt i : os) p of
--           Right (ds', os') -> go (i + increment)
--           Left err -> Left err
-- psFor _ _ = Left TypeMismatchError

-- psRepeat :: Operator
-- psRepeat ds (OperandInt limit : OperandProc p : os) = go limit
--   where
--     go :: Int -> Either InterpreterError OpResult
--     go 0 = Right (ds, os)
--     go n = case interpret ds os p of
--       Right (ds', os') -> go (n - 1)
--       Left err -> Left err
-- psRepeat _ _ = Left TypeMismatchError

-- psQuit :: Operator
-- psQuit _ _ = Left QuitError

-- {--#endregion Procedure Operations--}

-- | Length can be applied to either a dictionary or a string
psLength :: Operator
psLength ds (OperandDict d : os) = psLengthDict ds (OperandDict d : os)
psLength ds (OperandString s : os) = psStrLength ds (OperandString s : os)
psLength _ _ = Left TypeMismatchError