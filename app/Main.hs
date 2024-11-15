module Main (main) where

import Interpreter

main :: IO ()
main = do
  case interpretWithGlobalDict "true {1} if" of
    Right (_, os) -> print os
    Left err -> print err
