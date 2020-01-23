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

data Dec = VarDec TypeName [VarName] deriving Show
type VarName = String
type TypeName = String

nameLit :: Parser String
-- nameLit = do
--     h <- letter
--     r <- many (letter <|> digit)
--     return $ h : r
nameLit = (:) <$> letter <*> many (letter <|> digit) <* spaces

-- let name (, name)*
varDec :: Parser Dec
--letStmt = reserved "let" >> sepBy1 (token ",") nameLit
varDec = do
    reserved "var"
    typeName <- nameLit
    name1 <- nameLit
    names <- many (symbol "," >> nameLit)
    return $ VarDec typeName $ name1 : names
--letStmt = (:) <$> reserved "let" *> nameLit <*> many (symbol "," >> nameLit)

dec :: Parser Dec
dec = varDec <* symbol ";" 
