module Combinator where

import Parser
import AST

space :: Parser String
space = cComment <|> cppComment <|> (some $ oneOf " \n\r\t")

spaces = many space

token :: Parser a -> Parser a
token p = p <* spaces

reserved :: String -> Parser String
reserved str = token $ string str

reserveds :: [String] -> Parser String
reserveds [s] = reserved s
reserveds (s:r) = reserved s <|> reserveds r

parens :: Parser a -> Parser a
parens m = do
  reserved "("
  n <- m
  reserved ")"
  pure n

between :: String -> Parser a -> String -> Parser a
between begin p end = 
    reserved begin >> p <* reserved end

symbol :: String -> Parser String
symbol str = token $ string str

-- binOp :: String -> Parser (Expr -> Expr -> Expr)
-- binOp str = symbol str >> pure (BinOp str)

cppComment :: Parser String
cppComment = symbol "//" >> (endWith "\n" <|> many anyChar)

cComment :: Parser String
cComment = symbol "/*" >> endWith "*/"

nameLit :: Parser String
nameLit = (:) <$> letter <*> many (letter <|> digit) <* spaces

expr :: Parser Expr
expr = do
    t <- term
    ts <- many $ do
        o <- symbols op
        t2 <- term
        pure (o, t2)
    pure $ Expr t ts

symbols :: [String] -> Parser String
symbols [s] = symbol s
symbols (s:r) = symbol s <|> symbols r

op = ["+", "-", "*", "/", "&", "|", "<", ">", "="]

term :: Parser Term
term = subroutineCall
    <|> integerConstant
    <|> stringConstant
    <|> keywordConstant
    <|> arrayAccess    
    <|> paren
    <|> varName
    <|> unaryOp

paren :: Parser Term
paren = do
    e <- between "(" expr ")"
    pure $ Paren e

stringConstant :: Parser Term
stringConstant = do
    str <- between "\"" (many (noneOf "\"\n")) "\""
    pure $ StringConstant str

integerConstant :: Parser Term
integerConstant = IntegerConstant <$> token number

keywordConstant :: Parser Term
keywordConstant = do
    k <- reserveds ["true", "false", "nil", "this"]
    return $ KeywordConstant k

varName = TermIdentifier <$> identifier

arrayAccess :: Parser Term
arrayAccess = do
    v <- varName 
    e <- between "[" expr "]"
    pure $ ArrayAccess v e

identifier :: Parser Identifier
identifier = do
    f <- letter <|> char '_'
    r <- many (letter <|> char '_' <|> digit) <* spaces
    return $ Identifier $ f : r

unaryOp :: Parser Term
unaryOp = do
    u <- symbol "-" <|> symbol "~"
    t <- term
    pure $ UnaryOp u t

subroutineCall :: Parser Term
subroutineCall = 
    do
        s <- subroutineName
        es <- between "(" expressionList ")"
        pure $ SubroutineCall Nothing (TermIdentifier s) es
    <|>
    do
        n <- identifier
        symbol "." 
        s <- subroutineName
        es <- between "(" expressionList ")"
        pure $ SubroutineCall (Just (TermIdentifier n)) (TermIdentifier s) es

className = identifier
subroutineName = identifier

expressionList :: Parser [Expr]
expressionList = manyWith (symbol ",") expr

statement :: Parser Stmt
statement = doStatement
    <|> returnStatement
    <|> whileStatement
    <|> ifStatement
    <|> letStatement

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

statements = many statement


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
    case index of
        Just indexExpr -> pure $ Let (ArrayAccess v indexExpr) e
        Nothing        -> pure $ Let v e

subroutineBody :: Parser SubroutineBody
subroutineBody = do
    symbol "{"
    vs <- many varDec
    stmts <- statements
    symbol "}"
    pure $ SubroutineBody vs stmts

varDec :: Parser VarDec
varDec = do
    reserved "var"
    type' <- typeKeyword <|> (TermIdentifier <$> identifier)
    varName1 <- TermIdentifier <$> identifier
    varNames <- many (symbol "," >> TermIdentifier <$> identifier)
    symbol ";"
    return $ VarDec type' $ varName1 : varNames

typeKeyword :: Parser Term
typeKeyword = do
    k <- reserved "int" <|> reserved "char" <|> reserved "boolean" 
    return $ Keyword k

accessKeyword :: Parser Term
accessKeyword = do
    k <- reserveds ["field", "static"]
    return $ Keyword k

keyword :: Parser Term
keyword = do
    k <- reserveds ["class", "constructor", "function", "method", "field", "static", "var",
            "int", "char", "boolean", "void", "true", "false", "null", "this", "let", "do",
            "if", "else", "while", "return" ]
    return $ Keyword k    

classVarDec :: Parser ClassVarDec
classVarDec = do
    access <- reserveds ["static", "field"]
    type' <- typeKeyword <|> (TermIdentifier <$> identifier)
    varName1 <- TermIdentifier <$> identifier
    varNames <- many (symbol "," >> TermIdentifier <$> identifier)
    symbol ";"
    return $ ClassVarDec (Keyword access) type' $ varName1 : varNames

subroutineDec :: Parser SubroutineDec
subroutineDec = do
    subRoutineType <- reserveds ["constructor", "function", "method"]
    returnType <- (Keyword <$> reserved "void") <|> typeKeyword <|> (TermIdentifier <$> identifier)
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