module XmlGen where

import Combinator

tag :: String -> String -> String
tag tagName elmt = 
    "  <" ++ tagName ++ "> " ++ elmt ++ " </" ++ tagName ++ ">\n"

xmlGenVarDec :: VarDec -> String
xmlGenVarDec (VarDec typeName identifiers) = 
    "<varDec>\n" ++
    "  <keyword> var </keyword>\n" ++
    xmlGenName typeName ++
    foldr (\i z -> tag "symbol" "," ++ xmlGenName i ++ z) "" identifiers ++
    tag "symbol" ";"


xmlGenName :: Name -> String
xmlGenName (Identifier i) = tag "identifier" i
xmlGenName (Keyword k) = tag "keyword" k