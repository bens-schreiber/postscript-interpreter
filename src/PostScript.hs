module PostScript (interpPostScript, globalDictionary) where

import Dictionary (Dictionary, InterpreterError, OpResult, dictFromList)
import Interpreter (interpret)
import Operators

-- | Contains all default PostScript symbols and their corresponding operators
globalDictionary :: Dictionary
globalDictionary =
  dictFromList 100 $ stackOps ++ arithmeticOps ++ stringOps ++ booleanOps ++ dictOps ++ flowOps ++ [("length", psLength)]
  where
    stackOps = [("exch", psExch), ("pop", psPop), ("copy", psCopy), ("dup", psDup), ("clear", psClear), ("count", psCount)]
    arithmeticOps = [("add", psAdd), ("sub", psSub), ("mul", psMul), ("div", psDiv), ("mod", psMod)]
    stringOps = [("get", psGet), ("getinterval", psGetInterval), ("putinterval", psPutInterval)]
    booleanOps = [("eq", psEq), ("ne", psNe), ("and", psAnd), ("or", psOr), ("not", psNot), ("gt", psGt), ("lt", psLt)]
    dictOps = [("dict", psDict), ("maxlength", psMaxlength), ("begin", psBeginDict), ("end", psEndDict), ("def", psDef)]
    flowOps = [("if", psIf), ("ifelse", psIfElse), ("for", psFor), ("repeat", psRepeat), ("quit", psQuit)]

-- | Interprets a PostScript program, returning the final operand stack or an error
interpPostScript :: String -> Either InterpreterError OpResult
interpPostScript = interpret [globalDictionary] []