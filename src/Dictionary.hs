module Dictionary
  ( Operand (..),
    OpResult,
    InterpreterError (..),
    Operator,
    Dictionary (..),
    dict,
    dictLookup,
    dictStackLookup,
    dictFromList,
    tableSize,
    tableInsert
  )
where

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap

-- | Primitive value (operands) in PostScript
data Operand
  = OperandInt Int
  | OperandDouble Double
  | OperandBool Bool
  | OperandString String
  | OperandDict Dictionary
  | OperandName String
  | OperandProc String
  | OperandOut String
  deriving (Eq, Ord)

instance Show Operand where
  show (OperandInt i) = show i
  show (OperandDouble f) = show f
  show (OperandBool b) = if b then "true" else "false"
  show (OperandString s) = s
  show (OperandDict d) = show d
  show (OperandName n) = n
  show (OperandProc _) = "--nostringval--"
  show (OperandOut s) = s

-- | The result of an operation. Contains the new dictionary and operand stack.
type OpResult = ([Dictionary], [Operand])

data InterpreterError
  = SymbolNotFound String
  | StringNeverClosed
  | StringNeverOpened
  | ProcNeverClosed
  | ProcNeverOpened
  | GlobalDictionaryPopError
  | TypeMismatchError
  | StackUnderflowError
  | DivisionByZeroError
  | SqrtNegativeError
  | IndexOutOfBoundsError
  | QuitError
  deriving (Show, Eq)

-- | Some function that operates on both the operand stack and the dictionary stack
type Operator = [Dictionary] -> [Operand] -> Either InterpreterError OpResult

type SymbolTable = HashMap String Operator

-- | A table mapping some symbol to an operator function
data Dictionary = Dictionary
  { capacity :: Int,
    hashmap :: SymbolTable
  }

instance Eq Dictionary where
  (Dictionary cap1 _) == (Dictionary cap2 _) = cap1 == cap2

instance Show Dictionary where
  show (Dictionary _ _) = "--nostringval--"

instance Ord Dictionary where
  compare (Dictionary cap1 _) (Dictionary cap2 _) = compare cap1 cap2

-- | Create a dictionary with a given capacity
dict :: Int -> Dictionary
dict n = Dictionary n HashMap.empty

dictFromList :: Int -> [(String, Operator)] -> Dictionary
dictFromList n = Dictionary n . HashMap.fromList

tableSize :: SymbolTable -> Int
tableSize = HashMap.size

dictLookup :: String -> Dictionary -> Maybe Operator
dictLookup key (Dictionary _ d) = HashMap.lookup key d

tableInsert :: String -> Operator -> SymbolTable -> SymbolTable
tableInsert = HashMap.insert

-- | Lookup a symbol in a list of dictionaries. From the top of the stack to the bottom. First match wins.
dictStackLookup :: String -> [Dictionary] -> Maybe Operator
dictStackLookup _ [] = Nothing
dictStackLookup key (d : ds) = case dictLookup key d of
  Just op -> Just op
  Nothing -> dictStackLookup key ds