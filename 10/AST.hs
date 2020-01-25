module AST where

data Expr = KeywordConstant String 
    | IntegerConstant Int
    | StringConstant String
    | Identifier String
    | Keyword String
    | BinOp String Expr Expr
    | UnaryOp String Expr
    | Between String Expr String
    | SubroutineCall (Maybe Expr) Expr [Expr]
    deriving Show

data Stmt = Do Expr
    | Return (Maybe Expr)
    | While Expr [Stmt]
    | If Expr [Stmt] (Maybe [Stmt])
    | Let Expr (Maybe Expr) Expr
    deriving Show

