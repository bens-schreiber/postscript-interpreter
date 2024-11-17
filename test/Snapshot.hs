{-# LANGUAGE CPP #-}

module Snapshot (runSnapshotTests) where

import PostScript (interpPostScript)
import Test.HUnit

snapshotAllBasicOperators :: Test
snapshotAllBasicOperators = TestCase $ do
  in' <- readFile "test/sample/basic_ops.ps.in"
  out <- readFile "test/sample/basic_ops.out"

  case interpPostScript in' of
    Right (_, os) -> assertEqual "all basic operations" (lines out) (map show os)
    Left err -> assertFailure $ show err

snapshotFlowOps :: Test
snapshotFlowOps = TestCase $ do
  in' <- readFile "test/sample/flow_ops.ps.in"
  out <- readFile "test/sample/flow_ops.out"

  case interpPostScript in' of
    Right (_, os) -> assertEqual "all flow operations" (lines out) (map show os)
    Left err -> assertFailure $ show err

useStaticScoping :: Bool
#ifdef USE_STATIC_SCOPING
useStaticScoping = True
#else
useStaticScoping = False
#endif

snapshotScoping :: Test
snapshotScoping = TestCase $ do
  let inFile = "test/sample/scoping.ps.in"
  let outFile = if useStaticScoping then "test/sample/scoping.static.out" else "test/sample/scoping.dynamic.out"
  in' <- readFile inFile
  out <- readFile outFile

  case interpPostScript in' of
    Right (_, os) -> assertEqual ("Scoping " ++ (if useStaticScoping then "static" else "dynamic")) (lines out) (map show os)
    Left err -> assertFailure $ show err

runSnapshotTests :: IO ()
runSnapshotTests = do
  _ <-
    runTestTT $
      TestList
        [ snapshotAllBasicOperators,
          snapshotFlowOps,
          snapshotScoping
        ]
  return ()