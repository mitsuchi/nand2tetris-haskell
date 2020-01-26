module Combinator where

import Parser
import AST

space :: Parser String
space = cComment <|> cppComment <|> (some $ oneOf " \n\r\t")

spaces = many space

token :: Parser a -> Parser a
--token p = do { a <- p; spaces ; return a}
token p = p <* spaces

reserved :: String -> Parser String
reserved str = token $ string str

parens :: Parser a -> Parser a
parens m = do
  reserved "("
  n <- m
  reserved ")"
  pure n

between :: String -> Parser a -> String -> Parser a
between begin p end = 
    reserved begin >> p <* reserved end

-- data Expr = IntLit Int
--     | BinOp String Expr Expr
--     deriving Show

-- num :: Parser Expr
-- num = IntLit <$> token number

symbol :: String -> Parser String
symbol str = token $ string str

-- expr :: Parser Expr
-- expr = equality

binOp :: String -> Parser (Expr -> Expr -> Expr)
binOp str = symbol str >> pure (BinOp str)

-- equality :: Parser Expr
-- equality = 
--     relational `chainl1` ((binOp "==") <|> (binOp "!="))

-- relational :: Parser Expr
-- relational =
--     chainl1 add $
--         (binOp "<=")
--         <|> (binOp "<")
--         <|> (symbol ">=" >> pure (flip $ BinOp "<="))
--         <|> (symbol ">" >> pure (flip $ BinOp "<"))

-- add :: Parser Expr
-- add = 
--     mul `chainl1` ((binOp "+") <|> (binOp "-"))

-- mul :: Parser Expr
-- mul = do
--     unary `chainl1` ((binOp "*") <|> (binOp "/"))

-- unary :: Parser Expr
-- unary = 
--     (symbol "+" >> primary)
--     <|>
--     (BinOp "-" (IntLit 0) <$> (symbol "-" >> primary))
--     <|>
--     primary

-- primary :: Parser Expr
-- primary = (symbol "(" *> expr <* symbol ")") <|> num

-- program = spaces >> expr

cppComment :: Parser String
--cppComment = symbol "//" >> nonLineBreak >> lineBreak
cppComment = symbol "//" >> (endWith "\n" <|> many anyChar)

cComment :: Parser String
cComment = symbol "/*" >> endWith "*/"


nameLit :: Parser String
-- nameLit = do
--     h <- letter
--     r <- many (letter <|> digit)
--     return $ h : r
nameLit = (:) <$> letter <*> many (letter <|> digit) <* spaces

-- let name (, name)*
varDec :: Parser VarDec
--letStmt = reserved "let" >> sepBy1 (token ",") nameLit
varDec = do
    reserved "var"
--    typeName <- nameLit
    type' <- typeKeyword <|> identifier
    varName1 <- identifier
    varNames <- many (symbol "," >> identifier)
    symbol ";"
    return $ VarDec type' $ varName1 : varNames
--letStmt = (:) <$> reserved "let" *> nameLit <*> many (symbol "," >> nameLit)

typeKeyword :: Parser Expr
typeKeyword = do
    k <- reserved "int" <|> reserved "char" <|> reserved "boolean" 
    return $ Keyword k

accessKeyword :: Parser Expr
accessKeyword = do
    k <- reserveds ["field", "static"]
    return $ Keyword k

identifier :: Parser Expr
identifier = do
    f <- letter <|> char '_'
    r <- many (letter <|> char '_' <|> digit) <* spaces
    return $ Identifier $ f : r

keyword :: Parser Expr
keyword = do
    k <- reserveds ["class", "constructor", "function", "method", "field", "static", "var",
            "int", "char", "boolean", "void", "true", "false", "null", "this", "let", "do",
            "if", "else", "while", "return" ]
    return $ Keyword k    

reserveds :: [String] -> Parser String
reserveds [s] = reserved s
reserveds (s:r) = reserved s <|> reserveds r

classVarDec :: Parser ClassVarDec
classVarDec = do
    access <- reserveds ["static", "field"]
    type' <- typeKeyword <|> identifier
    varName1 <- identifier
    varNames <- many (symbol "," >> identifier)
    symbol ";"
    return $ ClassVarDec (Keyword access) type' $ varName1 : varNames

keywordConstant :: Parser Expr
keywordConstant = do
    k <- reserveds ["true", "false", "nil", "this"]
    return $ KeywordConstant k

unaryOp = symbol "~" <|> symbol "-"

symbols :: [String] -> Parser String
symbols [s] = symbol s
symbols (s:r) = symbol s <|> symbols r

op = ["+", "-", "*", "/", "&", "|", "<", ">", "="]

expressionList :: Parser [Expr]
expressionList = manyWith (symbol ",") expr

integerConstant :: Parser Expr
integerConstant = IntegerConstant <$> token number

stringConstant :: Parser Expr
stringConstant = do
    symbol "\""
    str <- many (noneOf "\"\n")
    symbol "\""
    pure $ StringConstant str

expr :: Parser Expr
expr = term `chainl1` (binOps op)

binOps :: [String] -> Parser (Expr -> Expr -> Expr)
binOps [s] = binOp s
binOps (s:r) = binOp s <|> binOps r

varName = identifier

term :: Parser Expr
term = subroutineCall
    <|> integerConstant 
    <|> stringConstant
    <|> keywordConstant
    <|> arrayAccess
    <|> varName
    <|> betweenExpr "(" expr ")"
    <|> unaryOpTerm

arrayAccess = do
    v <- varName 
    e <- betweenExpr "[" expr "]"
    pure $ ArrayAccess v e

betweenExpr :: String -> Parser Expr -> String -> Parser Expr
betweenExpr a e b = do
    reserved a
    e' <- e
    reserved b
    pure $ Between a e' b

unaryOpTerm :: Parser Expr
unaryOpTerm = do
    u <- symbol "-" <|> symbol "~"
    t <- term
    pure $ UnaryOp u t

subroutineCall :: Parser Expr
subroutineCall = 
    do
        s <- subroutineName
        es <- between "(" expressionList ")"
        pure $ SubroutineCall Nothing s es
    <|>
    do
        n <- identifier
        symbol "." 
        s <- subroutineName
        es <- between "(" expressionList ")"
        pure $ SubroutineCall (Just n) s es

className = identifier
subroutineName = identifier

-- statements

doStatement :: Parser Stmt
doStatement = do
    reserved "do"
    s <- subroutineCall
    symbol ";"
    pure $ Do s

returnStatement :: Parser Stmt
returnStatement = do
    reserved "return"
    e <- option expr
    symbol ";"
    pure $ Return e

whileStatement :: Parser Stmt
whileStatement = do
    reserved "while"
    e <- between "(" expr ")"
    stmts <- between "{" statements "}"
    pure $ While e stmts

ifStatement :: Parser Stmt
ifStatement = do
    reserved "if"
    cond <- between "(" expr ")"
    thenStmts <- between "{" statements "}"
    elseStmts <- option $ reserved "else" >> between "{" statements "}"
    pure $ If cond thenStmts elseStmts    

letStatement :: Parser Stmt
letStatement = do
    reserved "let"
    v <- varName
    index <- option $ between "[" expr "]"
    symbol "="
    e <- expr
    symbol ";"
    pure $ Let v index e

statements = many statement

statement :: Parser Stmt
statement = doStatement
    <|> returnStatement
    <|> whileStatement
    <|> ifStatement
    <|> letStatement

subroutineBody :: Parser SubroutineBody
subroutineBody = do
    symbol "{"
    vs <- many varDec
    stmts <- statements
    symbol "}"
    pure $ SubroutineBody vs stmts

subroutineDec :: Parser SubroutineDec
subroutineDec = do
    subRoutineType <- reserveds ["constructor", "function", "method"]
    returnType <- (Keyword <$> reserved "void") <|> typeKeyword <|> identifier
    sName <- subroutineName    
    pList <- between "(" (manyWith (symbol ",") param) ")"
    sBody <- subroutineBody
    pure $ SubroutineDec (Keyword subRoutineType) returnType sName pList sBody

param :: Parser Param
param = Param <$> typeKeyword <*> varName

klass :: Parser Klass
klass = do
    reserved "class"
    k <- className
    symbol "{"
    cvds <- many classVarDec
    subs <- many subroutineDec
    symbol "}"
    pure $ Klass k cvds subs