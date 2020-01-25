module AST where

data Expr = KeywordConstant String 
    | IntegerConstant Int
    | StringConstant String
    | Identifier String
    | Keyword String
    deriving Show
