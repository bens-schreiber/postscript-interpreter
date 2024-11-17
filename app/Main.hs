module Main (main) where

import Control.Monad
import Dictionary (Dictionary (..), InterpreterError (..), Operand (..))
import Interpreter (interpret)
import PostScript (globalDictionary)
import System.IO

-- | Enter a read-eval-print loop for PostScript code.
repl :: [Dictionary] -> [Operand] -> String -> IO ()
repl ds os code = do
  when (null code) $ do
    putStr "ps> "
    hFlush stdout

  postScriptCode <- getLine
  case interpret ds os (code ++ postScriptCode) of
    Right (ds', OperandOut o : os'') -> do
      putStrLn o
      repl ds' os'' ""
    Right (ds', os') -> repl ds' os' ""
    Left err -> case err of
      StringNeverClosed -> do
        repl ds os (code ++ postScriptCode ++ "\n")
      ProcNeverClosed -> do
        repl ds os (code ++ postScriptCode ++ "\n")
      QuitError -> do
        return ()
      _ -> do
        putStrLn $ "Execution error: " ++ show err
        repl ds os ""

main :: IO ()
main = repl [globalDictionary] [] ""