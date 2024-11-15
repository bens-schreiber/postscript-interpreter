module Dictionary where

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap

data Operand = OperandInt Int | OperandBool Bool | OperandString String | OperandDict Dictionary | OperandName String | OperandProc String
  deriving (Show, Eq, Ord)

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

-- | Some function that operates on both the operand os and the dictionary os, returning new stacks or an error
type Operator = [Dictionary] -> [Operand] -> Either InterpreterError OpResult

data Dictionary = Dictionary
  { capacity :: Int,
    hashmap :: HashMap String Operator
  }

-- \| A dictionary mapping some symbol to an implementation or value
instance Eq Dictionary where
  (Dictionary cap1 _) == (Dictionary cap2 _) = cap1 == cap2

instance Show Dictionary where
  show (Dictionary _ _) = "--nostringval--"

instance Ord Dictionary where
  compare (Dictionary cap1 _) (Dictionary cap2 _) = compare cap1 cap2

makeDict :: Int -> Dictionary
makeDict n = Dictionary n HashMap.empty

-- | Lookup a symbol in the dictionary
lookupDict :: String -> Dictionary -> Maybe Operator
lookupDict key (Dictionary _ d) = HashMap.lookup key d

lookupDictStackSymbol :: String -> [Dictionary] -> Maybe Operator
lookupDictStackSymbol key [] = Nothing
lookupDictStackSymbol key (d : ds) = case lookupDict key d of
  Just op -> Just op
  Nothing -> lookupDictStackSymbol key ds