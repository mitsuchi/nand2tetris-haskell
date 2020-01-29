module AST where

data Expr = Expr Term [(String, Term)]
    deriving Show

data Term = KeywordConstant String 
    | IntegerConstant Int
    | StringConstant String
    | TermIdentifier Identifier
    | Keyword String
    | UnaryOp String Term
    | SubroutineCall (Maybe Term) Term [Expr]
    | ArrayAccess Term Expr
    | Paren Expr
    deriving Show

data Stmt = Do Term
    | Return (Maybe Expr)
    | While Expr [Stmt]
    | If Expr [Stmt] (Maybe [Stmt])
    | Let Term Expr
    deriving Show

--data Class = Class Expr [ClassVarDec] [SubroutineDec]

data ClassVarDec = ClassVarDec AccessName TypeName [VarName] deriving Show

data VarDec = VarDec Term [Term] deriving Show
type TypeName = Term
type VarName = Term
type AccessName = Term

data SubroutineBody = SubroutineBody [VarDec] [Stmt] deriving Show
data SubroutineDec = SubroutineDec Term Term Identifier [Param] SubroutineBody deriving Show
data Param = Param Term Term deriving Show -- Type Nambe

data Klass = Klass Identifier [ClassVarDec] [SubroutineDec] deriving Show

data Identifier = Identifier String deriving Show