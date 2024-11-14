module Integration (runIntegrationTests) where

import GlobalDict
import Interpreter
import Test.HUnit

interpreterCanDoMultilineArithmetic :: Test
interpreterCanDoMultilineArithmetic =
  TestCase $
    case interpretWithGlobalDict input of
      Right (OpResult _ os) -> assertEqual "arithmetic" expected os
      Left err -> assertFailure $ show err
  where
    input =
      "\
      \1 2 add\n\
      \3 3 sub\n\
      \mul"
    expected = [OperandInt 0]

{--#region Error Handling--}
interpreterDoesNothingOnEmptyInput :: Test
interpreterDoesNothingOnEmptyInput =
  TestCase $
    case interpretWithGlobalDict "" of
      Right (OpResult _ os) -> assertEqual "empty input" [] os
      Left err -> assertFailure $ show err

interpreterDiscardsEmptySpaces :: Test
interpreterDiscardsEmptySpaces =
  TestCase $
    case interpretWithGlobalDict input of
      Right (OpResult _ os) -> assertEqual "empty spaces" expected os
      Left err -> assertFailure $ show err
  where
    input = "  \n  \t  "
    expected = []

interpreterRaisesErrorOnUnknownSymbol :: Test
interpreterRaisesErrorOnUnknownSymbol =
  TestCase $
    case interpretWithGlobalDict input of
      Left SymbolNotFound -> return ()
      Right _ -> assertFailure "Expected SymbolNotFound error"
      Left err -> assertEqual "Error" expected err
  where
    input = "1 2 foo"
    expected = SymbolNotFound

{--#endregion Error Handling--}

runIntegrationTests :: IO ()
runIntegrationTests = do
  _ <-
    runTestTT $
      TestList
        [ interpreterCanDoMultilineArithmetic,
          interpreterDoesNothingOnEmptyInput,
          interpreterDiscardsEmptySpaces,
          interpreterRaisesErrorOnUnknownSymbol
        ]
  return ()