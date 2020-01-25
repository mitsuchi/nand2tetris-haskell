module AST where

data Expr = KeywordConstant String 
    | IntegerConstant Int
    | StringConstant String
    | Identifier String
    | Keyword String
    | BinOp String Expr Expr
    deriving Show

