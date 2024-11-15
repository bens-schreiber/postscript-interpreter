module Main (main) where

import PostScript (interpPostScript)

main :: IO ()
main = do
  case interpPostScript "true {1} if" of
    Right (_, os) -> print os
    Left err -> print err
