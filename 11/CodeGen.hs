module CodeGen where

import AST
import Combinator
import Data.List

tag :: String -> String -> String
tag tagName elmt = "<" ++ tagName ++ "> " ++ escape(elmt) ++ " </" ++ tagName ++ ">\n"

escape :: String -> String
escape "<" = "&lt;"
escape ">" = "&gt;"
escape "&" = "&amp;"
escape x   = x

tagLn :: String -> String -> String
tagLn tagName elmt = 
    "<" ++ tagName ++ ">\n" ++ elmt ++ "</" ++ tagName ++ ">\n"

xmlGenName :: Name -> String
xmlGenName (Identifier i) = tag "identifier" i
xmlGenName (Keyword k) = tag "keyword" k

xmlGenExpr :: Expr -> String
xmlGenExpr (Expr t ts) = tagLn "expression" $ 
                            xmlGenTerm t ++
                            intercalate "" (map (\(s, t) -> tag "symbol" s ++ xmlGenTerm t) ts)

xmlGenTerm :: Term -> String
xmlGenTerm (IntegerConstant i) = tagLn "term" $ tag "integerConstant" (show i)
xmlGenTerm (TermIdentifier i) = tagLn "term" $ xmlGenName i
xmlGenTerm (KeywordConstant k) = tagLn "term" $ xmlGenName k
xmlGenTerm (StringConstant s) = tagLn "term" $ tag "stringConstant" s
xmlGenTerm (UnaryOp op t) =
        tagLn "term" $
            tag "symbol" op ++
            xmlGenTerm t
xmlGenTerm (ArrayAccess ary idx) =
    tagLn "term" $
        xmlGenName ary ++
        tag "symbol" "[" ++
        xmlGenExpr idx ++
        tag "symbol" "]"
xmlGenTerm (Paren e) =
    tagLn "term" $
        tag "symbol" "(" ++
        xmlGenExpr e ++
        tag "symbol" ")"
xmlGenTerm (TermSubroutineCall subCall) =
    tagLn "term" $ xmlGenSubroutineCall subCall

xmlGenSubroutineCall (SubroutineCall maybeClass func args) =
    let c = case maybeClass of
            Just cls -> xmlGenName cls ++ tag "symbol" "."
            Nothing -> ""
    in 
        c ++ xmlGenName func ++ 
        tag "symbol" "(" ++
        tagLn "expressionList" (intercalate (tag "symbol" ",") (map xmlGenExpr args)) ++
        tag "symbol" ")"

xmlGenStmt :: Stmt -> String
xmlGenStmt (Do subCall) = 
    tagLn "doStatement" $
        tag "keyword" "do" ++
        xmlGenSubroutineCall subCall ++
        tag "symbol" ";"
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
        tag "symbol" ")" ++
        tag "symbol" "{" ++
        xmlGenStmts thenStmts ++
        tag "symbol" "}" ++
        elseStmtsXml

-- Let Term Expr
xmlGenStmt (Let varTerm valExpr) =
    let varXml = case varTerm of
                    ArrayAccess v indexExpr -> xmlGenName v ++ tag "symbol" "[" ++ xmlGenExpr indexExpr ++ tag "symbol" "]"
                    TermIdentifier i -> xmlGenName i
    in tagLn "letStatement" $
        tag "keyword" "let" ++
        varXml ++
        tag "symbol" "=" ++
        xmlGenExpr valExpr ++
        tag "symbol" ";"

xmlGenStmts stmts = tagLn "statements" (foldr (\a s -> xmlGenStmt a ++ s) "" stmts)

-- data ClassVarDec = ClassVarDec AccessName TypeName [VarName] deriving Show
-- data ClassVarDec = ClassVarDec Term Term [Term] deriving Show
xmlGenClassVarDec (ClassVarDec accessName typeName varNames) =
    tagLn "classVarDec" $
        xmlGenName accessName ++
        xmlGenName typeName ++
        intercalate (tag "symbol" ",") (map xmlGenName varNames) ++
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
        xmlGenName typeName ++
        intercalate (tag "symbol" ",") (map xmlGenName termIdentifiers) ++
        tag "symbol" ";"

-- Param Term Term
xmlGenParam :: Param -> String
xmlGenParam (Param typeName valName) = 
    xmlGenName typeName ++
    xmlGenName valName

-- data SubroutineDec = SubroutineDec Term Term Identifier [Param] SubroutineBody deriving Show    
xmlGenSubroutineDec (SubroutineDec funcType returnType funcName params funcBody) = 
    tagLn "subroutineDec" $
        xmlGenName funcType ++
        xmlGenName returnType ++
        xmlGenName funcName ++
        tag "symbol" "(" ++
        tagLn "parameterList" (intercalate (tag "symbol" ",") (map xmlGenParam params)) ++
        tag "symbol" ")" ++
        xmlGenSubroutineBody funcBody

-- data Klass = Klass Identifier [ClassVarDec] [SubroutineDec] deriving Show        
xmlGenClass (Klass className classVarDecs subroutineDecs) =
    tagLn "class" $
        tag "keyword" "class" ++
        xmlGenName className ++
        tag "symbol" "{" ++
        (foldr (\a s -> xmlGenClassVarDec a ++ s) "" classVarDecs) ++
        (foldr (\a s -> xmlGenSubroutineDec a ++ s) "" subroutineDecs) ++
        tag "symbol" "}"
            