module XmlGen where

import Combinator
import AST

tag :: String -> String -> String
tag tagName elmt = 
    "<" ++ tagName ++ "> " ++ elmt ++ " </" ++ tagName ++ ">\n"

tagLn :: String -> String -> String
tagLn tagName elmt = 
    "<" ++ tagName ++ ">\n" ++ elmt ++ "</" ++ tagName ++ ">\n"

xmlGenVarDec :: VarDec -> String
xmlGenVarDec (VarDec typeName identifiers) = 
    "<varDec>\n" ++
    "<keyword> var </keyword>\n" ++
    xmlGenExpr typeName ++
    foldr (\i z -> tag "symbol" "," ++ xmlGenExpr i ++ z) "" identifiers ++
    tag "symbol" ";"


xmlGenExpr :: Expr -> String
xmlGenExpr (Identifier i) = tag "identifier" i
xmlGenExpr (Keyword k) = tag "keyword" k
xmlGenExpr (KeywordConstant k) = tag "keyword" k
xmlGenExpr (IntegerConstant i) = tag "integerConstant" (show i)
xmlGenExpr (Between begin e end) =
    tag "symbol" begin ++
    tagLn "expression" (xmlGenExpr e) ++
    tag "symbol" end
xmlGenExpr (BinOp op e1 e2) = 
    tagLn "expression" $
        tagLn "term" (xmlGenExpr e1) ++ 
        tag "symbol" op ++
        tagLn "term" (xmlGenExpr e2)
xmlGenExpr (UnaryOp op e) =
    tagLn "expression" $
        tagLn "term" $
            tag "symbol" op ++
            tagLn "term" (xmlGenExpr e)
xmlGenExpr (SubroutineCall maybeClass func args) =
    let c = case maybeClass of
                Just cls -> xmlGenExpr cls ++ tag "symbol" "."
                Nothing -> ""
    in tagLn "expression" $
            tagLn "term" $
                c ++ xmlGenExpr func ++ 
                tagLn "expressionList" (
                    foldr (\a s -> xmlGenExpr a ++ s) "" args)
xmlGenExpr (ArrayAccess ary idx) =
    xmlGenExpr ary ++
    tag "symbol" "[" ++
    tagLn "expression" (xmlGenExpr idx) ++
    tag "symbol" "]" 

xmlGenStmt :: Stmt -> String
xmlGenStmt (Do expr) = 
    tagLn "doStatement" $
        tag "keyword" "do" ++
        xmlGenExpr expr

xmlGenStmt (Return Nothing) = 
    tagLn "returnStatement" $
        tag "keyword" "return" ++
        tag "symbol" ";"

xmlGenStmt (Return (Just expr)) = 
    tagLn "returnStatement" $
        tag "keyword" "return" ++
        xmlGenExpr expr ++
        tag "symbol" ";"

xmlGenStmt (While expr stmts) =
    tagLn "whileStatement" $
        tag "keyword" "while" ++
        tag "symbol" "{" ++
        tagLn "expression" (xmlGenExpr expr) ++
        tagLn "statements" (
            foldr (\a s -> xmlGenStmt a ++ s) "" stmts)


        

