import Data.Maybe (isJust, isNothing)
import GlobalDict
import Test.HUnit

globalDictLookupSymbolReturnsJust :: Test
globalDictLookupSymbolReturnsJust =
  TestCase $
    assertBool "Operator not found" (isJust $ lookupDict "add" globalDictionary)

globalDictLookupSymbolReturnsNothing :: Test
globalDictLookupSymbolReturnsNothing =
  TestCase $
    assertBool "Operator found" (isNothing $ lookupDict "foo" globalDictionary)

globalDictEmptyOperandStackReturnsUnderflowError :: Test
globalDictEmptyOperandStackReturnsUnderflowError =
  TestCase $
    case lookupDict "add" globalDictionary of
      Just op -> assertEqual "add" (Left StackUnderflowError) (op [])
      Nothing -> assertFailure "Operator not found"

globalDictMismatchedTypesReturnsError :: Test
globalDictMismatchedTypesReturnsError =
  TestCase $
    case lookupDict "add" globalDictionary of
      Just op -> assertEqual "add" (Left $ TypeMismatchError "Expected two integers, but got: OperandInt 1 and OperandBool True") (op [OperandInt 1, OperandBool True])
      Nothing -> assertFailure "Operator not found"

{--#region Arithmetic--}
globalDictAdd :: Test
globalDictAdd =
  TestCase $
    case lookupDict "add" globalDictionary of
      Just op -> assertEqual "add" expected (op input)
      Nothing -> assertFailure "Operator not found"
  where
    input = [OperandInt 1, OperandInt 2]
    expected = Right [OperandInt 3]

globalDictSub :: Test
globalDictSub =
  TestCase $
    case lookupDict "sub" globalDictionary of
      Just op -> assertEqual "sub" expected (op input)
      Nothing -> assertFailure "Operator not found"
  where
    input = [OperandInt 1, OperandInt 2]
    expected = Right [OperandInt 1]

{--#endregion Arithmetic--}

{--#region Stack Manipulation--}

globalDictExch :: Test
globalDictExch =
  TestCase $
    case lookupDict "exch" globalDictionary of
      Just op -> assertEqual "exch" expected (op input)
      Nothing -> assertFailure "Operator not found"
  where
    input = [OperandInt 1, OperandInt 2]
    expected = Right [OperandInt 2, OperandInt 1]

globalDictPop :: Test
globalDictPop =
  TestCase $
    case lookupDict "pop" globalDictionary of
      Just op -> assertEqual "pop" expected (op input)
      Nothing -> assertFailure "Operator not found"
  where
    input = [OperandInt 1, OperandInt 2]
    expected = Right [OperandInt 2]

globalDictCopy :: Test
globalDictCopy =
  TestCase $
    case lookupDict "copy" globalDictionary of
      Just op -> assertEqual "copy" expected (op input)
      Nothing -> assertFailure "Operator not found"
  where
    input = [OperandInt 2, OperandInt 2, OperandInt 1] -- Top of stack is first, copy first 2 elements => 2, 1 ; append to top of stack => 2, 1, 2, 1
    expected = Right [OperandInt 2, OperandInt 1, OperandInt 2, OperandInt 1]

globalDictDup :: Test
globalDictDup =
  TestCase $
    case lookupDict "dup" globalDictionary of
      Just op -> assertEqual "dup" expected (op input)
      Nothing -> assertFailure "Operator not found"
  where
    input = [OperandInt 1, OperandInt 2]
    expected = Right [OperandInt 1, OperandInt 1, OperandInt 2]

globalDictClear :: Test
globalDictClear =
  TestCase $
    case lookupDict "clear" globalDictionary of
      Just op -> assertEqual "clear" expected (op input)
      Nothing -> assertFailure "Operator not found"
  where
    input = [OperandInt 1, OperandInt 2]
    expected = Right []

globalDictCount :: Test
globalDictCount =
  TestCase $
    case lookupDict "count" globalDictionary of
      Just op -> assertEqual "count" expected (op input)
      Nothing -> assertFailure "Operator not found"
  where
    input = [OperandInt 0, OperandInt 0]
    expected = Right [OperandInt 2, OperandInt 0, OperandInt 0]

{--#endregion Stack Manipulation--}

{--#region String Operations--}

globalDictLength :: Test
globalDictLength =
  TestCase $
    case lookupDict "length" globalDictionary of
      Just op -> assertEqual "length" expected (op input)
      Nothing -> assertFailure "Operator not found"
  where
    input = [OperandString "hello"]
    expected = Right [OperandInt 5]

globalDictGet :: Test
globalDictGet =
  TestCase $
    case lookupDict "get" globalDictionary of
      Just op -> assertEqual "get" expected (op input)
      Nothing -> assertFailure "Operator not found"
  where
    input = [OperandInt 0, OperandString "hello"]
    expected = Right [OperandInt 104]

globalDictGetOutOfBounds :: Test
globalDictGetOutOfBounds =
  TestCase $
    case lookupDict "get" globalDictionary of
      Just op -> assertEqual "get" expected (op input)
      Nothing -> assertFailure "Operator not found"
  where
    input = [OperandInt 5, OperandString "hello"]
    expected = Left $ TypeMismatchError "Index out of bounds"

globalDictGetInterval :: Test
globalDictGetInterval =
  TestCase $
    case lookupDict "getinterval" globalDictionary of
      Just op -> assertEqual "getinterval" expected (op input)
      Nothing -> assertFailure "Operator not found"
  where
    input = [OperandInt 0, OperandInt 2, OperandString "hello"]
    expected = Right [OperandString "he"]

globalDictPutInterval :: Test
globalDictPutInterval =
  TestCase $
    case lookupDict "putinterval" globalDictionary of
      Just op -> assertEqual "putinterval" expected (op input)
      Nothing -> assertFailure "Operator not found"
  where
    input = [OperandString "he", OperandInt 0, OperandString "xxllo"]
    expected = Right [OperandString "hello"]

{--#endregion String Operations--}

main :: IO ()
main = do
  _ <-
    runTestTT $
      TestList
        [ globalDictLookupSymbolReturnsJust,
          globalDictLookupSymbolReturnsNothing,
          globalDictEmptyOperandStackReturnsUnderflowError,
          globalDictMismatchedTypesReturnsError,
          globalDictAdd,
          globalDictSub,
          globalDictExch,
          globalDictPop,
          globalDictCopy,
          globalDictDup,
          globalDictClear,
          globalDictCount,
          globalDictLength,
          globalDictGet,
          globalDictGetOutOfBounds,
          globalDictGetInterval,
          globalDictPutInterval
        ]
  return ()