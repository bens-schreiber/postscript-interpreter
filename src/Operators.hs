{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Operators where

import Data.Bits (Bits ((.&.), (.|.)), complement)
import Dictionary (Dictionary (..), InterpreterError (..), Operand (..), Operator, dict, tableInsert, tableSize)
import Interpreter (interpret) -- Flow control will call interpret on it's procedures

{--#region Stack Manipulation--}

-- | Exchange the top two elements of the operand stack
psExch :: Operator
psExch ds (x : y : os) = Right (ds, y : x : os)
psExch _ _ = Left StackUnderflowError

-- | Pop the top element of the operand stack
psPop :: Operator
psPop ds (_ : os) = Right (ds, os)
psPop _ _ = Left StackUnderflowError

-- | Copy the top n elements of the operand stack and append them to the top of the stack
psCopy :: Operator
psCopy ds (OperandInt n : os)
  | n <= length os = Right (ds, take n os ++ os)
  | otherwise = Left StackUnderflowError
psCopy _ _ = Left StackUnderflowError

-- | Duplicate the top element of the operand stack
psDup :: Operator
psDup ds (o : os) = Right (ds, o : o : os)
psDup _ _ = Left StackUnderflowError

psClear :: Operator
psClear ds _ = Right (ds, [])

-- | Appends the number of elements on the operand stack to the operand stack
psCount :: Operator
psCount ds os = Right (ds, OperandInt (length os) : os)

{--#endregion Stack Manipulation--}

{--#region Arithmetic Operations--}
binaryNumOp :: (Double -> Double -> Double) -> Operator
binaryNumOp op ds (OperandInt x : OperandInt y : os) = Right (ds, OperandInt (floor $ op (fromIntegral y) (fromIntegral x)) : os)
binaryNumOp op ds (OperandDouble x : OperandDouble y : os) = Right (ds, OperandDouble (y `op` x) : os)
binaryNumOp op ds (OperandInt x : OperandDouble y : os) = Right (ds, OperandDouble (y `op` fromIntegral x) : os)
binaryNumOp op ds (OperandDouble x : OperandInt y : os) = Right (ds, OperandDouble (fromIntegral y `op` x) : os)
binaryNumOp _ _ (_ : _ : _) = Left TypeMismatchError
binaryNumOp _ _ _ = Left StackUnderflowError

binaryIntOp :: (Int -> Int -> Int) -> Operator
binaryIntOp op ds (OperandInt x : OperandInt y : os) = Right (ds, OperandInt (y `op` x) : os)
binaryIntOp _ _ (_ : _ : _) = Left TypeMismatchError
binaryIntOp _ _ _ = Left StackUnderflowError

unaryNumOp :: (Double -> Double) -> Operator
unaryNumOp op ds (OperandInt x : os) = Right (ds, OperandInt (floor $ op (fromIntegral x)) : os)
unaryNumOp op ds (OperandDouble x : os) = Right (ds, OperandDouble (op x) : os)
unaryNumOp _ _ (_ : _) = Left TypeMismatchError
unaryNumOp _ _ _ = Left StackUnderflowError

unaryIntOp :: (Double -> Int) -> Operator
unaryIntOp op ds (OperandInt x : os) = Right (ds, OperandInt (op (fromIntegral x)) : os)
unaryIntOp op ds (OperandDouble x : os) = Right (ds, OperandInt (op x) : os)
unaryIntOp _ _ (_ : _) = Left TypeMismatchError
unaryIntOp _ _ _ = Left StackUnderflowError

psAdd, psSub, psMul :: Operator
psAdd = binaryNumOp (+)
psSub = binaryNumOp (-)
psMul = binaryNumOp (*)

psAbs, psRound, psCeil, psFloor, psNeg :: Operator
psAbs = unaryNumOp abs
psRound = unaryIntOp round
psCeil = unaryIntOp ceiling
psFloor = unaryIntOp floor
psNeg = unaryNumOp negate

psMod :: Operator
psMod = binaryIntOp mod

psIDiv :: Operator
psIDiv _ (OperandInt 0 : _) = Left DivisionByZeroError
psIDiv ds os = binaryIntOp div ds os

psDiv :: Operator
psDiv _ (OperandInt 0 : _) = Left DivisionByZeroError
psDiv ds (OperandInt x : OperandInt y : os) = binaryNumOp (/) ds (OperandDouble (fromIntegral x) : OperandDouble (fromIntegral y) : os) -- stupid
psDiv ds os = binaryNumOp (/) ds os

psSqrt :: Operator
psSqrt ds (OperandInt x : os)
  | x >= 0 = Right (ds, OperandDouble (sqrt $ fromIntegral x) : os)
  | otherwise = Left SqrtNegativeError
psSqrt ds (OperandDouble x : os)
  | x >= 0 = Right (ds, OperandDouble (sqrt x) : os)
  | otherwise = Left SqrtNegativeError
psSqrt _ (_ : _) = Left TypeMismatchError
psSqrt _ _ = Left StackUnderflowError

{--#endregion Arithmetic Operations--}

{--#region String Operations--}

-- | Gets the length of a PostScript string, excluding the outermost parentheses (e.g. "(hello)" => 5)
pslen :: String -> Int
pslen = subtract 2 . length

-- | Wraps a string in parentheses
wrapstr :: String -> String
wrapstr s = "(" ++ s ++ ")"

-- | Strips the outermost parentheses from a string (e.g. "(hello)" => "hello")
stripstr :: String -> String
stripstr = init . tail

psStrLength :: Operator
psStrLength ds (OperandString s : os) = Right (ds, OperandInt (pslen s) : os)
psStrLength _ (_ : _) = Left TypeMismatchError
psStrLength _ [] = Left StackUnderflowError

-- | Get the character at index n in a string (excluding the outermost parentheses)
psGet :: Operator
psGet ds (OperandInt n : OperandString s : os)
  | n >= 0 && n < pslen s = Right (ds, OperandInt (fromEnum (s !! (n + 1))) : os)
  | otherwise = Left IndexOutOfBoundsError
psGet _ (_ : _ : _) = Left TypeMismatchError
psGet _ _ = Left StackUnderflowError

-- | Get a substring of length count starting at index n in a string (excluding the outermost parentheses)
psGetInterval :: Operator
psGetInterval ds (OperandInt n : OperandInt count : OperandString s : os)
  | n >= 0 && n + count <= pslen s = Right (ds, OperandString (wrapstr $ take count (drop (n + 1) s)) : os)
  | otherwise = Left IndexOutOfBoundsError
psGetInterval _ (_ : _ : _ : _) = Left TypeMismatchError
psGetInterval _ _ = Left StackUnderflowError

-- | Replace a substring of length count starting at index n in a string with a replacement string (excluding the outermost parentheses)
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

-- | Boolean AND and OR operations. For integers, these are bitwise operations.
psAnd, psOr :: Operator
psAnd = binaryBoolOp (&&) (.&.)
psOr = binaryBoolOp (||) (.|.)

-- | Boolean NOT operation. For integers, this is a bitwise operation.
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

-- | Create a new dictionary with n capacity and push it onto the operand stack
psDict :: Operator
psDict ds (OperandInt n : os) = Right (ds, OperandDict (dict n) : os)
psDict _ _ = Left TypeMismatchError

-- | Appends the amount of key, value pairs in a dictionary to the operand stack
psLengthDict :: Operator
psLengthDict ds (OperandDict (Dictionary _ d) : os) = Right (ds, OperandInt (tableSize d) : os)
psLengthDict _ _ = Left TypeMismatchError

-- | Appends the maximum capacity of a dictionary to the operand stack
psMaxlength :: Operator
psMaxlength ds (OperandDict (Dictionary n _) : os) = Right (ds, OperandInt n : os)
psMaxlength _ _ = Left TypeMismatchError

-- | Pushes a new dictionary onto the dictionary stack
psBeginDict :: Operator
psBeginDict ds (OperandDict d : os) = Right (d : ds, os)
psBeginDict _ _ = Left TypeMismatchError

-- | Pops the top dictionary from the dictionary stack
psEndDict :: Operator
psEndDict (_ : ds) os
  | null ds = Left GlobalDictionaryPopError
  | otherwise = Right (ds, os)
psEndDict _ _ = Left StackUnderflowError

-- | An operator that pushes a value onto the operand stack
valueOperator :: Operand -> Operator
valueOperator o ds os = Right (ds, o : os)

-- | Define a key, value pair in a dictionary from an OperandName and some operand value
-- On overflow, just change the dictionary to be larger. I don't know why PostScript does this, it must be some old limitation.
psDef :: Operator
psDef (Dictionary n d : ds) (value : OperandName key : os)
  | tableSize d < n = Right (Dictionary n (tableInsert key (valueOperator value) d) : ds, os)
  | otherwise = Right (Dictionary (n + 1) (tableInsert key (valueOperator value) d) : ds, os)
psDef _ _ = Left TypeMismatchError

{--#endregion Dictionary Operations--}

{--#region Flow Control Operators --}

-- | Strip the outermost curly braces from a procedure e.g "{ code }" => " code "
stripProc :: String -> String
stripProc = init . tail

-- | If the boolean is true, call the interpreter on the code block.
psIf :: Operator
psIf ds (OperandProc p : OperandBool b : os) = if b then interpret ds os (stripProc p) else Right (ds, os)
psIf _ _ = Left TypeMismatchError

-- | If the boolean is true, call the interpreter on the first code block, otherwise call it on the second.
psIfElse :: Operator
psIfElse ds (OperandProc p2 : OperandProc p1 : OperandBool b : os) =
  if b then interpret ds os (stripProc p1) else interpret ds os (stripProc p2)
psIfElse _ _ = Left TypeMismatchError

-- | start increment end { code } for
psFor :: Operator
psFor ds (OperandProc p : OperandInt end : OperandInt increment : OperandInt start : os) = go start ds os
  where
    go i ds' os'
      | i > end = Right (ds', os')
      | otherwise = do
          (ds'', os'') <- interpret ds' os' (stripProc p)
          go (i + increment) ds'' os''
psFor _ _ = Left TypeMismatchError

-- | n { code } repeat
psRepeat :: Operator
psRepeat ds (OperandProc p : OperandInt n : os) = go n ds os
  where
    go i ds' os'
      | i == 0 = Right (ds', os')
      | otherwise = do
          (ds'', os'') <- interpret ds' os' (stripProc p)
          go (i - 1) ds'' os''
psRepeat _ _ = Left TypeMismatchError

{--#endregion Flow Control Operators--}

{--#region Print Operators--}

psOut :: Operator
psOut ds (o : os) = Right (ds, OperandOut (show o) : os)
psOut _ _ = Left StackUnderflowError

psOutStack :: Operator
psOutStack ds os = Right (ds, OperandOut (show os) : os)

{--#endregion Print Operators--}

-- | Replaces the stack with a QuitError
psQuit :: Operator
psQuit _ _ = Left QuitError

-- | Append the length of a dictionary or string to the operand stack (arrays are not supported, so this is just a wrapper around psLengthDict and psStrLength)
psLength :: Operator
psLength ds (OperandDict d : os) = psLengthDict ds (OperandDict d : os)
psLength ds (OperandString s : os) = psStrLength ds (OperandString s : os)
psLength _ _ = Left TypeMismatchError
