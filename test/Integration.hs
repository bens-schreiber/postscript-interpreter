module Integration (runIntegrationTests) where

import Dictionary (InterpreterError (..), Operand (..))
import PostScript (interpPostScript)
import Test.HUnit

interpreterCanDoMultilineArithmetic :: Test
interpreterCanDoMultilineArithmetic =
  TestCase $
    case interpPostScript input of
      Right (_, os) -> assertEqual "arithmetic" expected os
      Left err -> assertFailure $ show err
  where
    input =
      "\
      \1 2 add\n\
      \3 3 sub\n\
      \mul"
    expected = [OperandInt 0]

interpreterEntersProcedure :: Test
interpreterEntersProcedure =
  TestCase $
    case interpPostScript input of
      Right (_, os) -> assertEqual "procedure" expected os
      Left err -> assertFailure $ show err
  where
    input = "true { 1 2 add } if"
    expected = [OperandInt 3]

interpreterEntersNestedProcedure :: Test
interpreterEntersNestedProcedure =
  TestCase $
    case interpPostScript input of
      Right (_, os) -> assertEqual "nested procedure" expected os
      Left err -> assertFailure $ show err
  where
    input = "true { true { 1 2 add } if } if"
    expected = [OperandInt 3]

interpreterIfElseProcedure :: Test
interpreterIfElseProcedure =
  TestCase $
    case interpPostScript input of
      Right (_, os) -> assertEqual "if else" expected os
      Left err -> assertFailure $ show err
  where
    input = "false { 1 } { 2 } ifelse"
    expected = [OperandInt 2]

interpreterForLoopProcedure :: Test
interpreterForLoopProcedure =
  TestCase $
    case interpPostScript input of
      Right (_, os) -> assertEqual "for loop" expected os
      Left err -> assertFailure $ show err
  where
    input = "0 1 5 { 1 } for"
    expected = [OperandInt 1, OperandInt 1, OperandInt 1, OperandInt 1, OperandInt 1, OperandInt 1]

interpreterRepeatProcedure :: Test
interpreterRepeatProcedure =
  TestCase $
    case interpPostScript input of
      Right (_, os) -> assertEqual "repeat loop" expected os
      Left err -> assertFailure $ show err
  where
    input = "5 { 1 } repeat"
    expected = [OperandInt 1, OperandInt 1, OperandInt 1, OperandInt 1, OperandInt 1]

{--#region Error Handling--}
interpreterDoesNothingOnEmptyInput :: Test
interpreterDoesNothingOnEmptyInput =
  TestCase $
    case interpPostScript "" of
      Right (_, os) -> assertEqual "empty input" [] os
      Left err -> assertFailure $ show err

interpreterDiscardsEmptySpaces :: Test
interpreterDiscardsEmptySpaces =
  TestCase $
    case interpPostScript input of
      Right (_, os) -> assertEqual "empty spaces" expected os
      Left err -> assertFailure $ show err
  where
    input = "  \n  \t  "
    expected = []

interpreterHandlesNestedParentheses :: Test
interpreterHandlesNestedParentheses =
  TestCase $
    case interpPostScript input of
      Right (_, os) -> assertEqual "nested parentheses" expected os
      Left err -> assertFailure $ show err
  where
    input = "(()) length"
    expected = [OperandInt 2]

interpreterRaisesErrorOnUnknownSymbol :: Test
interpreterRaisesErrorOnUnknownSymbol =
  TestCase $
    case interpPostScript input of
      Left (SymbolNotFound "foo") -> return ()
      Right _ -> assertFailure "Expected SymbolNotFound error"
      Left err -> assertEqual "Error" expected err
  where
    input = "1 2 foo"
    expected = SymbolNotFound "foo"

interpreterRaisesErrorOnUnmatchedParentheses :: Test
interpreterRaisesErrorOnUnmatchedParentheses =
  TestCase $
    case interpPostScript input of
      Left StringNeverClosed -> return ()
      Right _ -> assertFailure "Expected StringNeverClosed error"
      Left err -> assertEqual "Error" expected err
  where
    input = "(1 2"
    expected = StringNeverClosed

interpreterRaisesErrorOnUnmatchedParentheses2 :: Test
interpreterRaisesErrorOnUnmatchedParentheses2 =
  TestCase $
    case interpPostScript input of
      Left StringNeverOpened -> return ()
      Right _ -> assertFailure "Expected StringNeverOpened error"
      Left err -> assertEqual "Error" expected err
  where
    input = "1 2)"
    expected = StringNeverOpened

{--#endregion Error Handling--}

runIntegrationTests :: IO ()
runIntegrationTests = do
  _ <-
    runTestTT $
      TestList
        [ interpreterCanDoMultilineArithmetic,
          interpreterDoesNothingOnEmptyInput,
          interpreterDiscardsEmptySpaces,
          interpreterHandlesNestedParentheses,
          interpreterRaisesErrorOnUnknownSymbol,
          interpreterRaisesErrorOnUnmatchedParentheses,
          interpreterRaisesErrorOnUnmatchedParentheses2,
          interpreterEntersProcedure,
          interpreterEntersNestedProcedure,
          interpreterForLoopProcedure,
          interpreterRepeatProcedure,
          interpreterIfElseProcedure
        ]
  return ()