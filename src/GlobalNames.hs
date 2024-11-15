module GlobalNames (globalNames) where

import Dictionary
import Operators

globalNames :: [(String, Operator)]
globalNames = stackOps ++ arithmeticOps ++ stringOps ++ booleanOps ++ dictOps ++ [("length", psLength)]
  where
    stackOps = [("exch", psExch), ("pop", psPop), ("copy", psCopy), ("dup", psDup), ("clear", psClear), ("count", psCount)]
    arithmeticOps = [("add", psAdd), ("sub", psSub), ("mul", psMul), ("div", psDiv), ("mod", psMod)]
    stringOps = [("get", psGet), ("getinterval", psGetInterval), ("putinterval", psPutInterval)]
    booleanOps = [("eq", psEq), ("ne", psNe), ("and", psAnd), ("or", psOr), ("not", psNot), ("gt", psGt), ("lt", psLt)]
    dictOps = [("dict", psDict), ("maxlength", psMaxlength), ("begin", psBeginDict), ("end", psEndDict), ("def", psDef)]
