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

data VarDec = VarDec Name [Name] deriving Show
data Name = Keyword String | Identifier String deriving Show
type TypeName = Name
type VarName = Name
type AccessName = Name

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
    typeS <- typeKeyword <|> identifier
    varName1 <- identifier
    varNames <- many (symbol "," >> identifier)
    symbol ";"
    return $ VarDec typeS $ varName1 : varNames
--letStmt = (:) <$> reserved "let" *> nameLit <*> many (symbol "," >> nameLit)

typeKeyword :: Parser Name
typeKeyword = do
    k <- reserved "int" <|> reserved "char" <|> reserved "boolean"
    return $ Keyword k

accessKeyword :: Parser Name
accessKeyword = do
    k <- reserveds ["field", "static"]
    return $ Keyword k

identifier :: Parser Name
identifier = do
    f <- letter <|> char '_'
    r <- many (letter <|> char '_' <|> digit) <* spaces
    return $ Identifier $ f : r

keyword :: Parser Name
keyword = do
    k <- reserveds ["class", "constructor", "function", "method", "field", "static", "var",
            "int", "char", "boolean", "void", "true", "false", "null", "this", "let", "do",
            "if", "else", "while", "return" ]
    return $ Keyword k    

reserveds :: [String] -> Parser String
reserveds [s] = reserved s
reserveds (s:r) = reserved s <|> reserveds r

data ClassVarDec = ClassVarDec AccessName TypeName [VarName]

classVarDec :: Parser ClassVarDec
classVarDec = do
    access <- reserveds ["static", "field"]
    typeK <- typeKeyword
    varName1 <- identifier
    varNames <- many (symbol "," >> identifier)
    symbol ";"
    return $ ClassVarDec (Keyword access) typeK $ varName1 : varNames