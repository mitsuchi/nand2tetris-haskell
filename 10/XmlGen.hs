module XmlGen where

import Combinator

tag :: String -> String -> String
tag tagName elmt = 
    "  <" ++ tagName ++ "> " ++ elmt ++ " </" ++ tagName ++ ">\n"

xmlGen :: Dec -> String
xmlGen (VarDec typeName varNames) = 
    "<varDec>\n" ++
    "  <keyword> var </keyword>\n" ++
    tag "identifier" typeName ++
    foldr (\v z -> tag "identifier" v ++ z) "" varNames
    
