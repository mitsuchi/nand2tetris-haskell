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

--data Class = Class Expr [ClassVarDec] [SubroutineDec]

data ClassVarDec = ClassVarDec AccessName TypeName [VarName]

data VarDec = VarDec Expr [Expr] deriving Show
type TypeName = Expr
type VarName = Expr
type AccessName = Expr

data SubroutineBody = SubroutineBody [VarDec] [Stmt] deriving Show
data SubroutineDec = SubroutineDec Expr Expr Expr [Param] SubroutineBody deriving Show
data Param = Param Expr Expr deriving Show -- Type Nambe

data Klass = Klass Expr [ClassVarDec] [SubroutineDec]