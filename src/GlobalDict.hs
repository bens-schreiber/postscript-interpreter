{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module GlobalDict where

import Data.Bits
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap

data Operand = OperandInt Int | OperandBool Bool | OperandString String | OperandDict Dictionary | OperandName String
  deriving (Show, Eq, Ord)

data OpError = TypeMismatchError | StackUnderflowError | DivisionByZeroError | IndexOutOfBoundsError | StackOverflowError
  deriving (Show, Eq)

-- | Result of an operator function
data OpResult = OpResult
  { dictionaries :: [Dictionary],
    operands :: [Operand]
  }
  deriving (Show, Eq)

-- | Some function that operates on both the operand os and the dictionary os, returning new stacks or an error
type Operator = [Dictionary] -> [Operand] -> Either OpError OpResult

-- | A dictionary mapping some symbol to an implementation or value
data Dictionary = Dictionary
  { capacity :: Int,
    hashmap :: HashMap String Operator
  }

instance Eq Dictionary where
  (Dictionary cap1 _) == (Dictionary cap2 _) = cap1 == cap2

instance Show Dictionary where
  show (Dictionary cap _) = "Dictionary with capacity " ++ show cap

instance Ord Dictionary where
  compare (Dictionary cap1 _) (Dictionary cap2 _) = compare cap1 cap2

makeDict :: Int -> Dictionary
makeDict n = Dictionary n HashMap.empty

{--#region Stack Manipulation--}
psExch :: Operator
psExch ds (x : y : os) = Right $ OpResult ds (y : x : os)
psExch _ _ = Left StackUnderflowError

psPop :: Operator
psPop ds (_ : os) = Right $ OpResult ds os
psPop _ _ = Left StackUnderflowError

psCopy :: Operator
psCopy ds (OperandInt n : os)
  | n <= length os = Right $ OpResult ds (take n os ++ os)
  | otherwise = Left StackUnderflowError
psCopy _ _ = Left StackUnderflowError

psDup :: Operator
psDup ds (o : os) = Right $ OpResult ds (o : o : os)
psDup _ _ = Left StackUnderflowError

psClear :: Operator
psClear ds _ = Right $ OpResult ds []

psCount :: Operator
psCount ds os = Right $ OpResult ds (OperandInt (length os) : os)

{--#endregion Stack Manipulation--}

{--#region Arithmetic Operations--}
binaryIntOp :: (Int -> Int -> Int) -> Operator
binaryIntOp op ds (OperandInt x : OperandInt y : os) = Right $ OpResult ds (OperandInt (y `op` x) : os)
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
psStrLength :: Operator
psStrLength ds (OperandString s : os) = Right $ OpResult ds (OperandInt (length s) : os)
psStrLength _ (_ : _) = Left TypeMismatchError
psStrLength _ [] = Left StackUnderflowError

psGet :: Operator
psGet ds (OperandInt n : OperandString s : os)
  | n >= 0 && n < length s = Right $ OpResult ds (OperandInt (fromEnum (s !! n)) : os)
  | otherwise = Left IndexOutOfBoundsError
psGet _ (_ : _ : _) = Left TypeMismatchError
psGet _ _ = Left StackUnderflowError

psGetInterval :: Operator
psGetInterval ds (OperandInt n : OperandInt count : OperandString s : os)
  | n >= 0 && n + count <= length s = Right $ OpResult ds (OperandString (take count (drop n s)) : os)
  | otherwise = Left IndexOutOfBoundsError
psGetInterval _ (_ : _ : _ : _) = Left TypeMismatchError
psGetInterval _ _ = Left StackUnderflowError

psPutInterval :: Operator
psPutInterval ds (OperandString replacement : OperandInt si : OperandString s : os)
  | si >= 0 && si + length replacement <= length s = Right $ OpResult ds (OperandString (take si s ++ replacement ++ drop (si + length replacement) s) : os)
  | otherwise = Left IndexOutOfBoundsError
psPutInterval _ (_ : _ : _ : _) = Left TypeMismatchError
psPutInterval _ _ = Left StackUnderflowError

{--#endregion String Operations--}

{--#region Boolean Operations--}
psEq :: Operator
psEq ds (x : y : os) = Right $ OpResult ds (OperandBool (x == y) : os)
psEq _ _ = Left StackUnderflowError

psNe :: Operator
psNe ds (x : y : os) = Right $ OpResult ds (OperandBool (x /= y) : os)
psNe _ _ = Left StackUnderflowError

binaryBoolOp :: (Bool -> Bool -> Bool) -> (Int -> Int -> Int) -> Operator
binaryBoolOp binOp _ ds (OperandBool x : OperandBool y : os) = Right $ OpResult ds (OperandBool (y `binOp` x) : os)
binaryBoolOp _ bitOp ds (OperandBool x : OperandInt y : os) = Right $ OpResult ds (OperandInt (y `bitOp` fromEnum x) : os)
binaryBoolOp _ bitOp ds (OperandInt x : OperandBool y : os) = Right $ OpResult ds (OperandInt (x `bitOp` fromEnum y) : os)
binaryBoolOp _ bitOp ds (OperandInt x : OperandInt y : os) = Right $ OpResult ds (OperandInt (y `bitOp` x) : os)
binaryBoolOp _ _ _ (_ : _ : _) = Left TypeMismatchError
binaryBoolOp _ _ _ _ = Left StackUnderflowError

psAnd, psOr :: Operator
psAnd = binaryBoolOp (&&) (.&.)
psOr = binaryBoolOp (||) (.|.)

psNot :: Operator
psNot ds (OperandBool b : os) = Right $ OpResult ds (OperandBool (not b) : os)
psNot ds (OperandInt b : os) = Right $ OpResult ds (OperandInt (complement b) : os)
psNot _ (_ : _) = Left TypeMismatchError
psNot _ _ = Left StackUnderflowError

comparisonOp :: (Operand -> Operand -> Bool) -> Operator
comparisonOp op ds (x : y : os) = Right $ OpResult ds (OperandBool (y `op` x) : os)
comparisonOp _ _ _ = Left StackUnderflowError

psGt, psLt :: Operator
psGt = comparisonOp (>)
psLt = comparisonOp (<)

{--#endregion Boolean Operations--}

{-- #region Dictionary Operations--}

psDict :: Operator
psDict ds (OperandInt n : os) = Right $ OpResult ds (OperandDict (makeDict n) : os)
psDict _ _ = Left TypeMismatchError

psLengthDict :: Operator
psLengthDict ds (OperandDict (Dictionary _ d) : os) = Right $ OpResult ds (OperandInt (HashMap.size d) : os)
psLengthDict _ _ = Left TypeMismatchError

psMaxlength :: Operator
psMaxlength ds (OperandDict (Dictionary n _) : os) = Right $ OpResult ds (OperandInt n : os)
psMaxlength _ _ = Left TypeMismatchError

psBeginDict :: Operator
psBeginDict ds (OperandDict d : os) = Right $ OpResult (d : ds) os
psBeginDict _ _ = Left TypeMismatchError

psEndDict :: Operator
psEndDict (_ : ds) os = Right $ OpResult ds os
psEndDict _ _ = Left StackUnderflowError

-- | Creates an operator that pushes a value onto the operand stack
valueOperator :: Operand -> Operator
valueOperator o ds os = Right $ OpResult ds (o : os)

psDef :: Operator
psDef (Dictionary n d : ds) (value : OperandName key : os)
  | HashMap.size d < n = Right $ OpResult (Dictionary n (HashMap.insert key (valueOperator value) d) : ds) os
  | otherwise = Left StackOverflowError
psDef _ _ = Left TypeMismatchError

{--#endregion Dictionary Operations--}

-- | Length can be applied to either a dictionary or a string
psLength :: Operator
psLength ds (OperandDict d : os) = psLengthDict ds (OperandDict d : os)
psLength ds (OperandString s : os) = psStrLength ds (OperandString s : os)
psLength _ _ = Left TypeMismatchError

-- | Lookup a symbol in the dictionary
lookupDict :: String -> Dictionary -> Maybe Operator
lookupDict key (Dictionary _ d) = HashMap.lookup key d

-- | Contains all PostScript operators
globalDictionary :: Dictionary
globalDictionary =
  Dictionary 100 $ HashMap.fromList $ stackOps ++ arithmeticOps ++ stringOps ++ booleanOps ++ dictOps ++ [("length", psLength)]
  where
    stackOps = [("exch", psExch), ("pop", psPop), ("copy", psCopy), ("dup", psDup), ("clear", psClear), ("count", psCount)]
    arithmeticOps = [("add", psAdd), ("sub", psSub), ("mul", psMul), ("div", psDiv), ("mod", psMod)]
    stringOps = [("get", psGet), ("getinterval", psGetInterval), ("putinterval", psPutInterval)]
    booleanOps = [("eq", psEq), ("ne", psNe), ("and", psAnd), ("or", psOr), ("not", psNot), ("gt", psGt), ("lt", psLt)]
    dictOps = [("dict", psDict), ("maxlength", psMaxlength), ("begin", psBeginDict), ("end", psEndDict), ("def", psDef)]