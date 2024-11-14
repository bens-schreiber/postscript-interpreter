module Snapshot (runSnapshotTests) where

import GlobalDict
import Interpreter
import Test.HUnit

extractValue :: Operand -> String
extractValue (OperandString s) = s
extractValue (OperandInt i) = show i
extractValue (OperandBool b) = if b then "true" else "false"
extractValue (OperandDict d) = show d
extractValue (OperandName n) = n

snapshotSample1ArithmeticBoolsStringsDicts :: Test
snapshotSample1ArithmeticBoolsStringsDicts = TestCase $ do
  in' <- readFile "test/sample/sample1.ps.in"
  out <- readFile "test/sample/sample1.ps.out"

  case interpretWithGlobalDict in' of
    Right (OpResult _ os) -> assertEqual "sample1 arithmetic bools strings dicts" (lines out) (map extractValue os)
    Left err -> assertFailure $ show err

runSnapshotTests :: IO ()
runSnapshotTests = do
  _ <-
    runTestTT $
      TestList
        [ snapshotSample1ArithmeticBoolsStringsDicts
        ]
  return ()