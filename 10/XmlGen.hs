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

xmlGenTerm :: Term -> String
xmlGenTerm (IntegerConstant i) = tagLn "term" $ tag "integerConstant" (show i)
xmlGenTerm (TermIdentifier i) = tagLn "term" $ xmlGenIdentifier i
xmlGenTerm (KeywordConstant k) = tagLn "term" $ tag "keyword" k
xmlGenTerm (StringConstant s) = tagLn "term" $ tag "stringConstant" s
xmlGenTerm (UnaryOp op t) =
        tagLn "term" $
            tag "symbol" op ++
            xmlGenTerm t
xmlGenTerm (ArrayAccess ary idx) =
    xmlGenTerm ary ++
    tag "symbol" "[" ++
    xmlGenExpr idx ++
    tag "symbol" "]"
xmlGenTerm (Paren e) =
    tag "symbol" "(" ++
    xmlGenExpr e ++
    tag "symbol" ")"
xmlGenTerm (SubroutineCall maybeClass func args) =
    let c = case maybeClass of
                Just cls -> xmlGenTerm cls ++ tag "symbol" "."
                Nothing -> ""
    in tagLn "term" $
            c ++ xmlGenTerm func ++ 
            tag "symbol" "(" ++
            tagLn "expressionList" (foldr (\a s -> xmlGenExpr a ++ s) "" args) ++
            tag "symbol" ")"


xmlGenIdentifier :: Identifier -> String
xmlGenIdentifier (Identifier i) = tag "identifier" i

xmlGenKeyword (Keyword k) = tag "keyword" k

xmlGenStmt :: Stmt -> String
xmlGenStmt (Do term) = 
    tagLn "doStatement" $
        tag "keyword" "do" ++
        xmlGenTerm term

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
        tag "symbol" "(" ++
        xmlGenExpr expr ++
        tag "symbol" ")" ++
        tag "symbol" "{" ++
        xmlGenStmts stmts ++ 
        tag "symbol" "}"

-- If Expr [Stmt] (Maybe [Stmt])            
xmlGenStmt (If expr thenStmts maybeElseStmts) = 
    let elseStmtsXml = case maybeElseStmts of 
                        Just elseStmts -> 
                            tag "keyword" "else" ++
                            tag "symbol" "{" ++
                            xmlGenStmts elseStmts ++
                            tag "symbol" "}"
                        Nothing -> ""
    in tagLn "ifStatement" $
        tag "keyword" "if" ++
        tag "symbol" "(" ++
        xmlGenExpr expr ++
        tag "symbol" "(" ++
        tag "symbol" "{" ++
        xmlGenStmts thenStmts ++
        tag "symbol" "}"

-- Let Term Expr
xmlGenStmt (Let varTerm valExpr) = 
    tagLn "letStatement" $
        tag "keyword" "let" ++
        xmlGenTerm varTerm ++
        tag "symbol" "=" ++
        xmlGenExpr valExpr ++
        tag "symbol" ";"

xmlGenStmts stmts = tagLn "statements" (foldr (\a s -> xmlGenStmt a ++ s) "" stmts)

-- data ClassVarDec = ClassVarDec AccessName TypeName [VarName] deriving Show
-- data ClassVarDec = ClassVarDec Term Term [Term] deriving Show
xmlGenClassVarDec (ClassVarDec accessName typeName varNames) =
    tagLn "classVarDec" $
        xmlGenTerm accessName ++
        xmlGenTerm typeName ++
        intercalate (tag "symbol" ",") (map xmlGenTerm varNames) ++
        tag "symbol" ";"

-- data SubroutineBody = SubroutineBody [VarDec] [Stmt] deriving Show
xmlGenSubroutineBody (SubroutineBody varDecs stmts) =
    tagLn "subroutineBody" $
        tag "symbol" "{" ++
        foldr (\a s -> xmlGenVarDec a ++ s) "" varDecs ++
        tagLn "statements" (foldr (\a s -> xmlGenStmt a ++ s) "" stmts) ++
        tag "symbol" "}"

-- data VarDec = VarDec Term [Term] deriving Show        
xmlGenVarDec :: VarDec -> String
xmlGenVarDec (VarDec typeName termIdentifiers) = 
    tagLn "varDec" $
        tag "keyword" "var" ++
        xmlGenTerm typeName ++
        intercalate (tag "symbol" ",") (map xmlGenTerm termIdentifiers) ++
        tag "symbol" ";"

-- Param Term Term
xmlGenParam :: Param -> String
xmlGenParam (Param typeTerm valTerm) = 
    xmlGenTerm typeTerm ++
    xmlGenTerm valTerm

-- data SubroutineDec = SubroutineDec Term Term Identifier [Param] SubroutineBody deriving Show    
xmlGenSubroutineDec (SubroutineDec funcType returnType funcName params funcBody) = 
    tagLn "subroutineDec" $
        xmlGenTerm funcType ++
        xmlGenTerm returnType ++
        xmlGenIdentifier funcName ++
        tag "symbol" "(" ++
        tagLn "parameterList" (foldr (\a s -> xmlGenParam a ++ s) "" params) ++
        tag "symbol" ")" ++
        xmlGenSubroutineBody funcBody

-- data Klass = Klass Identifier [ClassVarDec] [SubroutineDec] deriving Show        
xmlGenClass (Klass className classVarDecs subroutineDecs) =
    tagLn "class" $
        tag "keyword" "class" ++
        xmlGenIdentifier className ++
        tag "symbol" "{" ++
        (foldr (\a s -> xmlGenClassVarDec a ++ s) "" classVarDecs) ++
        (foldr (\a s -> xmlGenSubroutineDec a ++ s) "" subroutineDecs) ++
        tag "symbol" "}"
            