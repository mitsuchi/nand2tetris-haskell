module Compile where

import AST
import Combinator
import Data.Char
import Data.List
import qualified Data.Map as M
import SymbolTable
import State
import MonadTrans
import MonadState
import Debug.Trace

makeSymbolTableForClass :: Klass -> SymbolTable        
makeSymbolTableForClass (Klass className classVarDecs subroutineDecs) =
    makeSymbolTableForClass' classVarDecs M.empty

makeSymbolTableForClass' :: [ClassVarDec] -> SymbolTable -> SymbolTable
makeSymbolTableForClass' [] table = table
makeSymbolTableForClass' (dec:decs) table = 
    makeSymbolTableForClass' decs (addToTable dec table)

addToTable :: ClassVarDec -> SymbolTable -> SymbolTable
addToTable (ClassVarDec kind typeName vars) table =
    let t = stringOf typeName;
        k = stringOf kind
    in foldr (\v st -> define st (stringOf v) t k (varCount st k)) table (reverse vars)

makeSymbolTableForSubroutine :: SubroutineDec -> String -> SymbolTable
makeSymbolTableForSubroutine (SubroutineDec accessName _ _ params subroutineBody) classStr =
    let SubroutineBody varDecs stmts = subroutineBody;
        initialTable = case (stringOf accessName) of
            "function" -> M.empty
            "method" -> M.fromList [("this", SymbolRow {typeName=classStr, kind="argument", index=0})]
            "constructor" -> M.empty         
        st = makeSymbolTableForSubroutine' varDecs initialTable
    in makeSymbolTableForSubroutineArgs params st

makeSymbolTableForSubroutine' :: [VarDec] -> SymbolTable -> SymbolTable
makeSymbolTableForSubroutine' [] table = table
makeSymbolTableForSubroutine' (dec:decs) table = 
    makeSymbolTableForSubroutine' decs (addVarDecToTable dec table)    

addVarDecToTable :: VarDec -> SymbolTable -> SymbolTable
addVarDecToTable (VarDec typeName vars) table =
    let t = stringOf typeName;
        k = "var"
    in foldr (\v st -> define st (stringOf v) t k (varCount st k)) table (reverse vars)

makeSymbolTableForSubroutineArgs :: [Param] -> SymbolTable -> SymbolTable
makeSymbolTableForSubroutineArgs [] table = table
makeSymbolTableForSubroutineArgs (param:params) table = 
    makeSymbolTableForSubroutineArgs params (addParamToTable param table)

addParamToTable :: Param -> SymbolTable -> SymbolTable
addParamToTable (Param typeName varName) table =
    let t = stringOf typeName;
        v = stringOf varName;
        k = "argument"
    in define table v t k (varCount table k)

data Context = Context { whileCount :: Int, ifCount :: Int, symbolEnv :: SymbolEnv } deriving Show
type Compiler a = State Context a

updateIfCount :: Context -> Int -> Context
updateIfCount ctx count = Context {whileCount = whileCount ctx, ifCount = count, symbolEnv = symbolEnv ctx}

updateWhileCount :: Context -> Int -> Context
updateWhileCount ctx count = Context {whileCount = count, ifCount = ifCount ctx, symbolEnv = symbolEnv ctx}

updateSymbolEnv :: Context -> SymbolEnv -> Context
updateSymbolEnv ctx env = Context {whileCount = whileCount ctx, ifCount = ifCount ctx, symbolEnv = env}

compile :: Klass -> String
compile k = execState (compileClass k) (Context {whileCount = 0, ifCount = 0, symbolEnv = SymbolEnv { table = M.empty, outer = Nothing }})

compileClass :: Klass -> Compiler String
compileClass cls@(Klass className classVarDecs subroutineDecs) =
    do
        let stClass = makeSymbolTableForClass cls
            classSymbolEnv = SymbolEnv { table = stClass, outer = Nothing }
        put $ Context {whileCount = 0, ifCount = 0, symbolEnv = classSymbolEnv}
        vms <- mapMR (compileSubroutineDec (stringOf className)) subroutineDecs
        pure $ intercalate "" vms

compileSubroutineDec :: String -> SubroutineDec -> Compiler String
compileSubroutineDec classStr subr@(SubroutineDec funcType returnType funcName params funcBody) = do
    ctx <- get
    let subroutineSymbols = makeSymbolTableForSubroutine subr classStr
        SubroutineBody varDecs stmts = funcBody
        se = SymbolEnv { table = subroutineSymbols, outer = Just (symbolEnv ctx) }
    put $ Context { whileCount = 0, ifCount = 0, symbolEnv = se }
    stmtsVM <- mapMR compileStmt stmts
    pure $ "function " ++ classStr ++ "." ++ (stringOf funcName) ++ " " ++ (show $ numLocalVars funcBody) ++ "\n" ++
      intercalate "" stmtsVM
    where numLocalVars (SubroutineBody varDecs stmts) = foldr (\(VarDec _ vars) total -> length vars + total) 0 varDecs

mapMR :: (Monad m) => (a -> m b) -> [a] -> m [b]
mapMR f as = do
    bs <- mapM f (reverse as)
    pure $ (reverse bs)

compileStmt :: Stmt -> Compiler String
compileStmt (Let (ArrayAccess arrayName indexExpr) valExpr) = do
    indexVM <- compileExpr indexExpr
    arrayVM <- compileTerm (TermIdentifier arrayName)
    valVM <- compileExpr valExpr
    pure $ indexVM ++ arrayVM ++ "add\n" ++ valVM ++ "pop temp 0\npop pointer 1\npush temp 0\npop that 0\n"
compileStmt (Let ti@(TermIdentifier i) valExpr) = do    
    tiVM <- compileVal ti
    valVM <- compileExpr valExpr
    pure $ valVM ++ writePop tiVM            
compileStmt (Do subCall@(SubroutineCall _ _ args)) = do
     doVM <- compileSubroutineCall subCall
     pure $ doVM ++ "pop temp 0\n"
compileStmt (Return Nothing) = pure "push constant 0\nreturn\n"
compileStmt (Return (Just expr)) = do
    exprVM <- compileExpr expr
    pure $ exprVM ++ "return\n"
compileStmt (While expr stmts) = do
    ctx <- get
    wc <- pure $ whileCount ctx
    labelBegin <- pure $ "WHILE_EXP" ++ (show wc) ++ "\n"
    labelEnd <- pure $ "WHILE_END" ++ (show wc) ++ "\n"
    condVM <- compileExpr expr
    stmtVMs <- mapMR compileStmt stmts
    put $ updateWhileCount ctx (wc + 1) 
    pure $ "label " ++ labelBegin ++ 
           condVM ++ 
           "not\n" ++ 
           "if-goto " ++ labelEnd ++ 
           intercalate "" stmtVMs ++
           "goto " ++ labelBegin ++
           "label " ++ labelEnd
compileStmt (If expr thenStmts (Just elseStmts)) = do
    ctx <- get
    let count = ifCount ctx
        labelIfTrue = "IF_TRUE" ++ (show count) ++ "\n"
        labelIfFalse = "IF_FALSE" ++ (show count) ++ "\n"
        labelIfEnd = "IF_END" ++ (show count) ++ "\n"
    put $ updateIfCount ctx (count + 1)
    condVM <- compileExpr expr
    thenStmtVMs <- mapMR compileStmt thenStmts
    elseStmtVMs <- mapMR compileStmt elseStmts
    pure $ condVM ++
           "if-goto " ++ labelIfTrue ++
           "goto " ++ labelIfFalse ++
           "label " ++ labelIfTrue ++
           intercalate "" thenStmtVMs ++
           "goto " ++ labelIfEnd ++
           "label " ++ labelIfFalse ++
           intercalate "" elseStmtVMs ++
           "label " ++ labelIfEnd
compileStmt (If expr thenStmts Nothing) = do
    ctx <- get
    let count = ifCount ctx
        labelIfTrue = "IF_TRUE" ++ (show count) ++ "\n"
        labelIfFalse = "IF_FALSE" ++ (show count) ++ "\n"
    put $ updateIfCount ctx (count + 1)
    condVM <- compileExpr expr
    thenStmtVMs <- mapMR compileStmt thenStmts
    pure $ condVM ++
           "if-goto " ++ labelIfTrue ++
           "goto " ++ labelIfFalse ++
           "label " ++ labelIfTrue ++
           intercalate "" thenStmtVMs ++
           "label " ++ labelIfFalse
compileStmt x = error $ "compileStmt: " ++ show x

compileVal ::  Term -> Compiler String
compileVal (TermIdentifier ti) = valToStack (stringOf ti)
compileVal (IntegerConstant i) = pure $ "constant " ++ show i

valToStack :: String -> Compiler String
valToStack str = do
    ctx <- get
    let symbols = symbolEnv ctx
        region = case kindOf symbols str of
                        Just kind -> case kind of
                                        "field" -> "this"
                                        "var"   -> "local"
                                        x       -> x
                        Nothing -> "error" ;
        index = case indexOf symbols str of
                    Just i -> show i
                    Nothing -> "error" 
    pure $ region ++ " " ++ index

writePop :: String -> String
writePop name = "pop " ++ name ++ "\n"

compileExpr :: Expr -> Compiler String
compileExpr (Expr t ts) = (++) <$> compileTerm t <*> compileArithTerms ts
-- compileExpr (Expr t ts) = do
--     te <- compileTerm t
--     ts <- compileArithTerms ts
--     pure $ te ++ ts

compileTerm :: Term -> Compiler String
compileTerm ii@(IntegerConstant i) = writePush <$> (compileVal ii)
compileTerm ti@(TermIdentifier i) = writePush <$> (compileVal ti)
compileTerm (Paren e) = compileExpr e
compileTerm (TermSubroutineCall subCall) = compileSubroutineCall subCall
compileTerm (StringConstant s) = pure $
    "push constant " ++ (show $ length s) ++ "\n" ++
    "call String.new 1\n" ++
    intercalate "" (map (\c -> "push constant " ++ (show $ ord c) ++ "\ncall String.appendChar 2\n") s)
compileTerm (ArrayAccess arrayName indexExpr) = do
    indexVM <- compileExpr indexExpr
    arrayVM <- compileTerm (TermIdentifier arrayName)
    pure $ indexVM ++ arrayVM ++ "add\npop pointer 1\npush that 0\n"
compileTerm (KeywordConstant k) = case (stringOf k) of
    "true" -> pure "push constant 0\nnot\n"
    "false" -> pure "push constant 0\n"
    "null" -> pure "push constant 0\n"
    "this" -> pure "push pointer 0\n"
    "that" -> pure "push pointer 1\n"
    x -> error $ "compileTerm: " ++ x
compileTerm (UnaryOp op t) = do
    let opVM =
            case op of
                "~" -> "not"
                "-" -> "neg"
    termVM <- compileTerm t
    pure $ termVM ++ opVM ++ "\n"

writePush :: String -> String
writePush name = "push " ++ name ++ "\n"

compileArithTerms :: [(String, Term)] -> Compiler String
compileArithTerms [] = pure ""
compileArithTerms ((arithSymbol, term):ats) = do
    t <- compileTerm term
    a <- compileArithTerms ats
    s <- compileArith arithSymbol
    pure $ t ++ a ++ s

compileArith :: String -> Compiler String
compileArith "+" = pure "add\n"
compileArith "-" = pure "sub\n"
compileArith "*" = pure "call Math.multiply 2\n"
compileArith "/" = pure "call Math.divide 2\n"
compileArith "<" = pure "lt\n"
compileArith ">" = pure "gt\n"
compileArith "&" = pure "and\n"
compileArith "|" = pure "or\n"
compileArith "=" = pure "eq\n"
compileArith x = error $ "compileArith: " ++ x

compileSubroutineCall :: SubroutineCall -> Compiler String
compileSubroutineCall (SubroutineCall maybeClass func args) = do
    ctx <- get
    pushObject <- case maybeClass of
        Just cls -> compileTerm (TermIdentifier cls)
        Nothing -> pure ""
    let symbols = symbolEnv ctx
        classStr = case maybeClass of
                        Just cls -> case typeOf symbols (stringOf cls) of
                                        Just kls -> kls
                                        Nothing -> (stringOf cls)
                        Nothing -> case typeOf symbols "this" of
                                    Just cls' -> cls'
                                    Nothing -> "error"
        (argOffset, pushThis) = case maybeClass of
                        Just cls -> case indexOf symbols (stringOf cls) of
                                        Just index -> (1, pushObject)
                                        Nothing -> (0, "")
                        Nothing -> (1, "push argument 0\n")
    argsVM <- mapMR compileExpr args
    pure $ pushThis ++ 
        intercalate "" argsVM ++
        "call " ++ classStr ++ "." ++ stringOf func ++ " " ++ (show $ length args + argOffset) ++ "\n"
