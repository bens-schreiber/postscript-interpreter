module Main (main) where

import GlobalDict
import Interpreter

main :: IO ()
main = do
  case interpretWithGlobalDict "(abcd) 2 0 getinterval" of
    Right (OpResult _ os) -> print os
    Left err -> print err
