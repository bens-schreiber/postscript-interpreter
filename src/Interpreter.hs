module Interpreter where

import Data.Bits (Bits ((.&.), (.|.)), complement)
import Data.Char (isDigit)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap

-- | Primitive value (operands) in PostScript
data Operand
  = OperandInt Int
  | OperandBool Bool
  | OperandString String
  | OperandDict Dictionary
  | OperandName String
  | OperandProc String
  deriving (Show, Eq, Ord)

-- | The result of an operation. Contains the new dictionary and operand stack.
type OpResult = ([Dictionary], [Operand])

data InterpreterError
  = SymbolNotFound String
  | StringNeverClosed
  | StringNeverOpened
  | ProcNeverClosed
  | ProcNeverOpened
  | TypeMismatchError
  | StackUnderflowError
  | DivisionByZeroError
  | IndexOutOfBoundsError
  | StackOverflowError
  | QuitError
  deriving (Show, Eq)

-- | Some function that operates on both the operand stack and the dictionary stack
type Operator = [Dictionary] -> [Operand] -> Either InterpreterError OpResult

-- | A table mapping some symbol to an operator function
data Dictionary = Dictionary
  { capacity :: Int,
    hashmap :: HashMap String Operator
  }

instance Eq Dictionary where
  (Dictionary cap1 _) == (Dictionary cap2 _) = cap1 == cap2

instance Show Dictionary where
  show (Dictionary _ _) = "--nostringval--"

instance Ord Dictionary where
  compare (Dictionary cap1 _) (Dictionary cap2 _) = compare cap1 cap2

-- | Create a dictionary with a given capacity
makeDict :: Int -> Dictionary
makeDict n = Dictionary n HashMap.empty

-- | Lookup a symbol in a dictionary
lookupDict :: String -> Dictionary -> Maybe Operator
lookupDict key (Dictionary _ d) = HashMap.lookup key d

-- | Lookup a symbol in a list of dictionaries. From the top of the stack to the bottom. First match wins.
lookupDictStackSymbol :: String -> [Dictionary] -> Maybe Operator
lookupDictStackSymbol _ [] = Nothing
lookupDictStackSymbol key (d : ds) = case lookupDict key d of
  Just op -> Just op
  Nothing -> lookupDictStackSymbol key ds

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
psDict ds (OperandInt n : os) = Right (ds, OperandDict (makeDict n) : os)
psDict _ _ = Left TypeMismatchError

-- | Appends the amount of key, value pairs in a dictionary to the operand stack
psLengthDict :: Operator
psLengthDict ds (OperandDict (Dictionary _ d) : os) = Right (ds, OperandInt (HashMap.size d) : os)
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
psEndDict (_ : ds) os = Right (ds, os)
psEndDict _ _ = Left StackUnderflowError

-- | An operator that pushes a value onto the operand stack
valueOperator :: Operand -> Operator
valueOperator o ds os = Right (ds, o : os)

-- | Define a key, value pair in a dictionary from an OperandName and some operand value
psDef :: Operator
psDef (Dictionary n d : ds) (value : OperandName key : os)
  | HashMap.size d < n = Right (Dictionary n (HashMap.insert key (valueOperator value) d) : ds, os)
  | otherwise = Left StackOverflowError
psDef _ _ = Left TypeMismatchError

{--#endregion Dictionary Operations--}

{-- #region Procedure Operations--}

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

-- | Returns a QuitError stopping the interpreter
psQuit :: Operator
psQuit _ _ = Left QuitError

{--#endregion Procedure Operations--}

{--# region Array Operations --}

-- | Append the length of a dictionary or string to the operand stack (arrays are not supported, so this is just a wrapper around psLengthDict and psStrLength)
psLength :: Operator
psLength ds (OperandDict d : os) = psLengthDict ds (OperandDict d : os)
psLength ds (OperandString s : os) = psStrLength ds (OperandString s : os)
psLength _ _ = Left TypeMismatchError

{--# endregion Array Operations--}

-- | Contains all default PostScript symbols and their corresponding operators
globalDictionary :: Dictionary
globalDictionary =
  Dictionary 100 $ HashMap.fromList $ stackOps ++ arithmeticOps ++ stringOps ++ booleanOps ++ dictOps ++ flowOps ++ [("length", psLength)]
  where
    stackOps = [("exch", psExch), ("pop", psPop), ("copy", psCopy), ("dup", psDup), ("clear", psClear), ("count", psCount)]
    arithmeticOps = [("add", psAdd), ("sub", psSub), ("mul", psMul), ("div", psDiv), ("mod", psMod)]
    stringOps = [("get", psGet), ("getinterval", psGetInterval), ("putinterval", psPutInterval)]
    booleanOps = [("eq", psEq), ("ne", psNe), ("and", psAnd), ("or", psOr), ("not", psNot), ("gt", psGt), ("lt", psLt)]
    dictOps = [("dict", psDict), ("maxlength", psMaxlength), ("begin", psBeginDict), ("end", psEndDict), ("def", psDef)]
    flowOps = [("if", psIf), ("ifelse", psIfElse), ("for", psFor), ("repeat", psRepeat), ("quit", psQuit)]

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
-- Tokenize the code, then process each token.
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
      | isName token = Right (ds, OperandName (tail token) : os')
      | isProc token = Right (ds, OperandProc token : os')
      | otherwise = case lookupDictStackSymbol token ds' of
          Just op -> case op ds' os' of
            Right (ds'', os'') -> Right (ds'', os'')
            Left err -> Left err
          Nothing -> Left (SymbolNotFound token)
    isStr, isBool, isName, isProc :: String -> Bool
    isStr s = head s == '(' && last s == ')'
    isBool s = s == "true" || s == "false"
    isName s = head s == '/'
    isProc s = head s == '{' && last s == '}'
    isInt = all isDigit

-- | An interpreter initialized with the global dictionary and an empty operand stack.
interpretWithGlobalDict :: String -> Either InterpreterError OpResult
interpretWithGlobalDict = interpret [globalDictionary] []