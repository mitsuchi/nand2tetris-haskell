module Combinator where

import Parser

data Expr = IntLit Int
    | BinOp String Expr Expr
    deriving Show

num :: Parser Expr
num = IntLit <$> token number

symbol :: String -> Parser String
symbol str = token $ string str

expr :: Parser Expr
expr = equality

binOp :: String -> Parser (Expr -> Expr -> Expr)
binOp str = symbol str >> pure (BinOp str)

equality :: Parser Expr
equality = 
    relational `chainl1` ((binOp "==") <|> (binOp "!="))

relational :: Parser Expr
relational =
    chainl1 add $
        (binOp "<=")
        <|> (binOp "<")
        <|> (symbol ">=" >> pure (flip $ BinOp "<="))
        <|> (symbol ">" >> pure (flip $ BinOp "<"))

add :: Parser Expr
add = 
    mul `chainl1` ((binOp "+") <|> (binOp "-"))

mul :: Parser Expr
mul = do
    unary `chainl1` ((binOp "*") <|> (binOp "/"))

unary :: Parser Expr
unary = 
    (symbol "+" >> primary)
    <|>
    (BinOp "-" (IntLit 0) <$> (symbol "-" >> primary))
    <|>
    primary

primary :: Parser Expr
primary = (symbol "(" *> expr <* symbol ")") <|> num

program = spaces >> expr

cppComment :: Parser String
--cppComment = symbol "//" >> nonLineBreak >> lineBreak
cppComment = symbol "//" >> (endWith "\n" <|> many anyChar)

cComment :: Parser String
cComment = symbol "/*" >> endWith "*/"

data Stmt = LetStmt [Name] deriving Show
type Name = String

nameLit :: Parser Name
nameLit = do
    h <- letter
    r <- many (letter <|> digit)
    return $ h : r

-- let name (, name)*
letStmt :: Parser Stmt
--letStmt = reserved "let" >> sepBy1 (token ",") nameLit
letStmt = do
    reserved "let"
    name1 <- nameLit
    names <- many (symbol "," >> nameLit)
    return $ LetStmt $ name1 : names

stmt :: Parser Stmt
stmt = letStmt <* symbol ";" 
