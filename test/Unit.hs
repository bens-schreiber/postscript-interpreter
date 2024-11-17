module Unit (runUnitTests) where

import Data.Maybe (isJust, isNothing)
import Dictionary (InterpreterError (..), Operand (..), dict, dictLookup)
import Operators
import PostScript (globalDictionary)
import Test.HUnit
  ( Test (TestCase, TestList),
    assertBool,
    assertEqual,
    assertFailure,
    runTestTT,
  )

globalDictLookupSymbolReturnsJust :: Test
globalDictLookupSymbolReturnsJust =
  TestCase $
    assertBool "Operator not found" (isJust $ dictLookup "add" globalDictionary)

globalDictLookupSymbolReturnsNothing :: Test
globalDictLookupSymbolReturnsNothing =
  TestCase $
    assertBool "Operator found" (isNothing $ dictLookup "foo" globalDictionary)

globalDictEmptyOperandStackReturnsUnderflowError :: Test
globalDictEmptyOperandStackReturnsUnderflowError =
  TestCase $
    case dictLookup "add" globalDictionary of
      Just op -> assertEqual "add" (Left StackUnderflowError) (op [] [])
      _ -> assertFailure "Operator not found"

globalDictMismatchedTypesReturnsError :: Test
globalDictMismatchedTypesReturnsError =
  TestCase $
    case dictLookup "add" globalDictionary of
      Just op -> assertEqual "add" (Left TypeMismatchError) (op [] [OperandInt 1, OperandBool True])
      _ -> assertFailure "Operator not found"

{--#region Arithmetic--}
testPsAdd :: Test
testPsAdd =
  TestCase $
    assertEqual "add" expected (psAdd ds os)
  where
    ds = []
    os = [OperandInt 1, OperandInt 2]
    expected = Right ([], [OperandInt 3])

testPsSub :: Test
testPsSub =
  TestCase $
    assertEqual "sub" expected (psSub ds os)
  where
    ds = []
    os = [OperandInt 1, OperandInt 2]
    expected = Right ([], [OperandInt 1])

{--#endregion Arithmetic--}

{--#region Stack Manipulation--}

testPsExch :: Test
testPsExch =
  TestCase $
    assertEqual "exch" expected (psExch ds os)
  where
    ds = []
    os = [OperandInt 1, OperandInt 2]
    expected = Right ([], [OperandInt 2, OperandInt 1])

testPsPop :: Test
testPsPop =
  TestCase $
    assertEqual "pop" expected (psPop ds os)
  where
    ds = []
    os = [OperandInt 1, OperandInt 2]
    expected = Right ([], [OperandInt 2])

testPsCopy :: Test
testPsCopy =
  TestCase $
    assertEqual "copy" expected (psCopy ds os)
  where
    ds = []
    os = [OperandInt 2, OperandInt 2, OperandInt 1] -- Top of stack is first, copy first 2 elements => 2, 1 ; append to top of stack => 2, 1, 2, 1
    expected = Right ([], [OperandInt 2, OperandInt 1, OperandInt 2, OperandInt 1])

testPsDup :: Test
testPsDup =
  TestCase $
    assertEqual "dup" expected (psDup ds os)
  where
    ds = []
    os = [OperandInt 1, OperandInt 2]
    expected = Right ([], [OperandInt 1, OperandInt 1, OperandInt 2])

testPsClear :: Test
testPsClear =
  TestCase $
    assertEqual "clear" expected (psClear ds os)
  where
    ds = []
    os = [OperandInt 1, OperandInt 2]
    expected = Right ([], [])

testPsCount :: Test
testPsCount =
  TestCase $
    assertEqual "count" expected (psCount ds os)
  where
    ds = []
    os = [OperandInt 0, OperandInt 0]
    expected = Right ([], [OperandInt 2, OperandInt 0, OperandInt 0])

{--#endregion Stack Manipulation--}

{--#region String Operations--}

testPsLength :: Test
testPsLength =
  TestCase $
    assertEqual "length" expected (psLength ds os)
  where
    ds = []
    os = [OperandString "(hello)"]
    expected = Right ([], [OperandInt 5])

testPsGet :: Test
testPsGet =
  TestCase $
    assertEqual "get" expected (psGet ds os)
  where
    ds = []
    os = [OperandInt 0, OperandString "(hello)"]
    expected = Right ([], [OperandInt 104])

testPsGetOutOfBounds :: Test
testPsGetOutOfBounds =
  TestCase $
    assertEqual "get" expected (psGet ds os)
  where
    ds = []
    os = [OperandInt 5, OperandString "(hello)"]
    expected = Left IndexOutOfBoundsError

testPsGetInterval :: Test
testPsGetInterval =
  TestCase $
    assertEqual "getinterval" expected (psGetInterval ds os)
  where
    ds = []
    os = [OperandInt 0, OperandInt 2, OperandString "(hello)"]
    expected = Right ([], [OperandString "(he)"])

testPsPutInterval :: Test
testPsPutInterval =
  TestCase $
    assertEqual "putinterval" expected (psPutInterval ds os)
  where
    ds = []
    os = [OperandString "(he)", OperandInt 0, OperandString "(xxllo)"]
    expected = Right ([], [OperandString "(hello)"])

{--#endregion String Operations--}

{--#region Boolean Operations--}

testPsEq :: Test
testPsEq =
  TestCase $
    assertEqual "eq" expected (psEq ds os)
  where
    ds = []
    os = [OperandInt 1, OperandInt 1]
    expected = Right ([], [OperandBool True])

testPsNe :: Test
testPsNe =
  TestCase $
    assertEqual "ne" expected (psNe ds os)
  where
    ds = []
    os = [OperandInt 1, OperandInt 1]
    expected = Right ([], [OperandBool False])

testPsGt :: Test
testPsGt =
  TestCase $
    assertEqual "gt" expected (psGt ds os)
  where
    ds = []
    os = [OperandInt 1, OperandInt 2]
    expected = Right ([], [OperandBool True])

testPsLt :: Test
testPsLt =
  TestCase $
    assertEqual "lt" expected (psLt ds os)
  where
    ds = []
    os = [OperandInt 1, OperandInt 2]
    expected = Right ([], [OperandBool False])

testPsAnd :: Test
testPsAnd =
  TestCase $
    assertEqual "and" expected (psAnd ds os)
  where
    ds = []
    os = [OperandBool True, OperandBool False]
    expected = Right ([], [OperandBool False])

testPsOr :: Test
testPsOr =
  TestCase $
    assertEqual "or" expected (psOr ds os)
  where
    ds = []
    os = [OperandBool True, OperandBool False]
    expected = Right ([], [OperandBool True])

testPsNot :: Test
testPsNot =
  TestCase $
    assertEqual "not" expected (psNot ds os)
  where
    ds = []
    os = [OperandBool True]
    expected = Right ([], [OperandBool False])

{--#endregion Boolean Operations--}

{--#region Dictionary Operations--}

testDict :: Test
testDict =
  TestCase $
    assertEqual "dict" expected (psDict ds os)
  where
    ds = []
    os = [OperandInt 5]
    expected = Right ([], [OperandDict $ dict 5])

testLengthDict :: Test
testLengthDict =
  TestCase $
    assertEqual "lengthdict" expected (psLengthDict ds os)
  where
    ds = []
    os = [OperandDict $ dict 5]
    expected = Right ([], [OperandInt 0])

testMaxLength :: Test
testMaxLength =
  TestCase $
    assertEqual "maxlength" expected (psMaxlength ds os)
  where
    ds = []
    os = [OperandDict $ dict 5]
    expected = Right ([], [OperandInt 5])

testBeginDict :: Test
testBeginDict =
  TestCase $
    assertEqual "begindict" expected (psBeginDict ds os)
  where
    ds = []
    os = [OperandDict $ dict 5]
    expected = Right ([dict 5], [])

testEndDict :: Test
testEndDict =
  TestCase $
    assertEqual "enddict" expected (psEndDict ds os)
  where
    ds = [dict 5, dict 3]
    os = []
    expected = Right ([dict 3], [])

testDef :: Test
testDef =
  TestCase $
    case psDef ds os of
      Right (ds', os') -> case dictLookup "foo" (head ds') of
        Just op -> assertEqual "def" expected (op ds' os')
        Nothing -> assertFailure "foo not found"
      Left _ -> assertFailure "Error"
  where
    ds = [dict 5]
    os = [OperandInt 1, OperandName "foo"]
    expected = Right (ds, [OperandInt 1])

{--#endregion Dictionary Operations--}

runUnitTests :: IO ()
runUnitTests = do
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
          testPsNot,
          testDict,
          testLengthDict,
          testMaxLength,
          testBeginDict,
          testEndDict,
          testDef
        ]
  return ()