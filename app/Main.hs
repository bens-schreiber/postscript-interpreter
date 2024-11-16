module Main (main) where

import PostScript (interpPostScript)

main :: IO ()
main = do
  let postScriptCode = "/outervar 1 def\n\n/func {\n    /outervar 2 def\n    outervar\n} def\n\nfunc\noutervar\n"
  case interpPostScript postScriptCode of
    Right (_, os) -> print os
    Left err -> print err
