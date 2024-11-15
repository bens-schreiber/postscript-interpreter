module Main (main) where

import Interpreter

main :: IO ()
main = do
  case interpretWithGlobalDict "(abcd) 2 0 getinterval" of
    Right (_, os) -> print os
    Left err -> print err
