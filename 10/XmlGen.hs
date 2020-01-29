module XmlGen where

import AST
import Combinator
import Data.List

tag :: String -> String -> String
tag tagName elmt = 
    "<" ++ tagName ++ "> " ++ elmt ++ " </" ++ tagName ++ ">\n"

tagLn :: String -> String -> String
tagLn tagName elmt = 
    "<" ++ tagName ++ ">\n" ++ elmt ++ "</" ++ tagName ++ ">\n"

xmlGenExpr :: Expr -> String
xmlGenExpr (Expr t ts) = tagLn "expression" $ 
                            xmlGenTerm t ++
                            intercalate "" (map (\(s, t) -> tag "op" s ++ xmlGenTerm t) ts)

xmlGenTerm (IntegerConstant i) = tagLn "term" $ tag "integerConstant" (show i)
