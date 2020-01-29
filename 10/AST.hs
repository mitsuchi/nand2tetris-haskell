module AST where

data Name = Keyword String
    | Identifier String
    deriving Show

data Expr = Expr Term [(String, Term)]
    deriving Show

data Term = KeywordConstant Name 
    | IntegerConstant Int
    | StringConstant String
    | TermIdentifier Name
    | UnaryOp String Term
    | TermSubroutineCall SubroutineCall
    | ArrayAccess Name Expr
    | Paren Expr
    deriving Show

data Stmt = Do SubroutineCall
    | Return (Maybe Expr)
    | While Expr [Stmt]
    | If Expr [Stmt] (Maybe [Stmt])
    | Let Term Expr
    deriving Show

type Stmts = [Stmt]

--data Class = Class Expr [ClassVarDec] [SubroutineDec]

data ClassVarDec = ClassVarDec AccessName TypeName [VarName] deriving Show

data VarDec = VarDec Name [Name] deriving Show
type TypeName = Name
type VarName = Name
type AccessName = Name

data SubroutineBody = SubroutineBody [VarDec] [Stmt] deriving Show
data SubroutineDec = SubroutineDec Name Name Name [Param] SubroutineBody deriving Show
data Param = Param Name Name deriving Show -- Type Nambe

data Klass = Klass Name [ClassVarDec] [SubroutineDec] deriving Show

--                                   className   funcName args
data SubroutineCall = SubroutineCall (Maybe Name) Name [Expr] deriving Show
