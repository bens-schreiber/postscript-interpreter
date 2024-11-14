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

snapshotAllBasicOperators :: Test
snapshotAllBasicOperators = TestCase $ do
  in' <- readFile "test/sample/basic_ops.ps.in"
  out <- readFile "test/sample/basic_ops.out"

  case interpretWithGlobalDict in' of
    Right (OpResult _ os) -> assertEqual "all basic operations" (lines out) (map extractValue os)
    Left err -> assertFailure $ show err

runSnapshotTests :: IO ()
runSnapshotTests = do
  _ <-
    runTestTT $
      TestList
        [ snapshotAllBasicOperators
        ]
  return ()