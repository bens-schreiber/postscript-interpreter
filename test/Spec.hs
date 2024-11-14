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
testPsAdd :: Test
testPsAdd =
  TestCase $
    assertEqual "add" expected (psAdd input)
  where
    input = [OperandInt 1, OperandInt 2]
    expected = Right [OperandInt 3]

testPsSub :: Test
testPsSub =
  TestCase $
    assertEqual "sub" expected (psSub input)
  where
    input = [OperandInt 1, OperandInt 2]
    expected = Right [OperandInt 1]

{--#endregion Arithmetic--}

{--#region Stack Manipulation--}

testPsExch :: Test
testPsExch =
  TestCase $
    assertEqual "exch" expected (psExch input)
  where
    input = [OperandInt 1, OperandInt 2]
    expected = Right [OperandInt 2, OperandInt 1]

testPsPop :: Test
testPsPop =
  TestCase $
    assertEqual "pop" expected (psPop input)
  where
    input = [OperandInt 1, OperandInt 2]
    expected = Right [OperandInt 2]

testPsCopy :: Test
testPsCopy =
  TestCase $
    assertEqual "copy" expected (psCopy input)
  where
    input = [OperandInt 2, OperandInt 2, OperandInt 1] -- Top of stack is first, copy first 2 elements => 2, 1 ; append to top of stack => 2, 1, 2, 1
    expected = Right [OperandInt 2, OperandInt 1, OperandInt 2, OperandInt 1]

testPsDup :: Test
testPsDup =
  TestCase $
    assertEqual "dup" expected (psDup input)
  where
    input = [OperandInt 1, OperandInt 2]
    expected = Right [OperandInt 1, OperandInt 1, OperandInt 2]

testPsClear :: Test
testPsClear =
  TestCase $
    assertEqual "clear" expected (psClear input)
  where
    input = [OperandInt 1, OperandInt 2]
    expected = Right []

testPsCount :: Test
testPsCount =
  TestCase $
    assertEqual "count" expected (psCount input)
  where
    input = [OperandInt 0, OperandInt 0]
    expected = Right [OperandInt 2, OperandInt 0, OperandInt 0]

{--#endregion Stack Manipulation--}

{--#region String Operations--}

testPsLength :: Test
testPsLength =
  TestCase $
    assertEqual "length" expected (psLength input)
  where
    input = [OperandString "hello"]
    expected = Right [OperandInt 5]

testPsGet :: Test
testPsGet =
  TestCase $
    assertEqual "get" expected (psGet input)
  where
    input = [OperandInt 0, OperandString "hello"]
    expected = Right [OperandInt 104]

testPsGetOutOfBounds :: Test
testPsGetOutOfBounds =
  TestCase $
    assertEqual "get" expected (psGet input)
  where
    input = [OperandInt 5, OperandString "hello"]
    expected = Left IndexOutOfBoundsError

testPsGetInterval :: Test
testPsGetInterval =
  TestCase $
    assertEqual "getinterval" expected (psGetInterval input)
  where
    input = [OperandInt 0, OperandInt 2, OperandString "hello"]
    expected = Right [OperandString "he"]

testPsPutInterval :: Test
testPsPutInterval =
  TestCase $
    assertEqual "putinterval" expected (psPutInterval input)
  where
    input = [OperandString "he", OperandInt 0, OperandString "xxllo"]
    expected = Right [OperandString "hello"]

{--#endregion String Operations--}

{--#region Boolean Operations--}

testPsEq :: Test
testPsEq =
  TestCase $
    assertEqual "eq" expected (psEq input)
  where
    input = [OperandInt 1, OperandInt 1]
    expected = Right [OperandBool True]

testPsNe :: Test
testPsNe =
  TestCase $
    assertEqual "ne" expected (psNe input)
  where
    input = [OperandInt 1, OperandInt 1]
    expected = Right [OperandBool False]

testPsGt :: Test
testPsGt =
  TestCase $
    assertEqual "gt" expected (psGt input)
  where
    input = [OperandInt 1, OperandInt 2]
    expected = Right [OperandBool True]

testPsLt :: Test
testPsLt =
  TestCase $
    assertEqual "lt" expected (psLt input)
  where
    input = [OperandInt 1, OperandInt 2]
    expected = Right [OperandBool False]

testPsAnd :: Test
testPsAnd =
  TestCase $
    assertEqual "and" expected (psAnd input)
  where
    input = [OperandBool True, OperandBool False]
    expected = Right [OperandBool False]

testPsOr :: Test
testPsOr =
  TestCase $
    assertEqual "or" expected (psOr input)
  where
    input = [OperandBool True, OperandBool False]
    expected = Right [OperandBool True]

testPsNot :: Test
testPsNot =
  TestCase $
    assertEqual "not" expected (psNot input)
  where
    input = [OperandBool True]
    expected = Right [OperandBool False]

{--#endregion Boolean Operations--}

main :: IO ()
main = do
  _ <-
    runTestTT $
      TestList
        [ globalDictLookupSymbolReturnsJust,
          globalDictLookupSymbolReturnsNothing,
          globalDictEmptyOperandStackReturnsUnderflowError,
          globalDictMismatchedTypesReturnsError,
          testPsAdd,
          testPsSub,
          testPsExch,
          testPsPop,
          testPsCopy,
          testPsDup,
          testPsClear,
          testPsCount,
          testPsLength,
          testPsGet,
          testPsGetOutOfBounds,
          testPsGetInterval,
          testPsPutInterval,
          testPsEq,
          testPsNe,
          testPsGt,
          testPsLt,
          testPsAnd,
          testPsOr,
          testPsNot
        ]
  return ()