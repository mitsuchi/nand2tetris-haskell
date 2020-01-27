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

-- If Expr [Stmt] (Maybe [Stmt])            
xmlGenStmt (If expr thenStmts maybeElseStmts) = 
    let elseStmtsXml = case maybeElseStmts of 
                        Just elseStmts -> 
                            tag "keyword" "else" ++
                            tag "symbol" "{" ++
                            tagLn "statements" (foldr (\a s -> xmlGenStmt a ++ s) "" elseStmts) ++
                            tag "symbol" "}"
                        Nothing -> ""
    in tagLn "ifStatement" $
        tag "keyword" "if" ++
        tag "symbol" "(" ++
        tagLn "expression" (xmlGenExpr expr) ++
        tag "symbol" "(" ++
        tag "symbol" "{" ++
        tagLn "statements" (foldr (\a s -> xmlGenStmt a ++ s) "" thenStmts) ++
        tag "symbol" "}"

-- Let Expr (Maybe Expr) Expr
xmlGenStmt (Let varExpr valExpr) = 
    tag "keyword" "let" ++
    xmlGenExpr varExpr ++
    tag "symbol" "=" ++
    xmlGenExpr valExpr ++
    tag "symbol" ";"

-- static boolean test; 
-- data ClassVarDec = ClassVarDec AccessName TypeName [VarName]
xmlGenClassVarDec (ClassVarDec accessName typeName varNames) =
    tagLn "classVarDec" $
        xmlGenExpr accessName ++
        xmlGenExpr typeName ++
        intercalate (tag "symbol" ",") (map xmlGenExpr varNames) ++
        tag "symbol" ";"

-- SubroutineBody [VarDec] [Stmt]
xmlGenSubroutineBody (SubroutineBody varDecs stmts) =
    tagLn "subroutineBody" $
        tag "symbol" "{" ++
        foldr (\a s -> xmlGenVarDec a ++ s) "" varDecs ++
        tagLn "statements" (foldr (\a s -> xmlGenStmt a ++ s) "" stmts) ++
        tag "symbol" "}"


-- Param Expr Expr
xmlGenParam (Param typeExpr valExpr) = 
    xmlGenExpr typeExpr ++
    xmlGenExpr valExpr

-- function void main (int i, int j) { body }
-- data SubroutineDec = SubroutineDec Expr Expr Expr [Param] SubroutineBody
xmlGenSubroutineDec (SubroutineDec funcType returnType funcName params funcBody) = 
    tagLn "subroutineDec" $
        xmlGenExpr funcType ++
        xmlGenExpr returnType ++
        xmlGenExpr funcName ++
        tag "symbol" "(" ++
        tagLn "parameterList" (foldr (\a s -> xmlGenParam a ++ s) "" params) ++
        tag "symbol" ")" ++
        tag "symbol" "{" ++
        xmlGenSubroutineBody funcBody ++ 
        tag "symbol" "}"

-- data Klass = Klass Expr [ClassVarDec] [SubroutineDec]
xmlGenClass (Klass className classVarDecs subroutineDecs) =
    tagLn "class" $
        tag "keyword" "class" ++
        xmlGenExpr className ++
        tag "symbol" "{" ++
        (foldr (\a s -> xmlGenClassVarDec a ++ s) "" classVarDecs) ++
        (foldr (\a s -> xmlGenSubroutineDec a ++ s) "" subroutineDecs) ++
        tag "symbol" "}"
    