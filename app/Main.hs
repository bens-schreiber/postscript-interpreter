module Main (main) where

import GlobalDict
import Interpreter

main :: IO ()
main = do
  print $
    tokenize
      "  \n  \t  "
