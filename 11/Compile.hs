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

data Context = Context { whileCount :: Int, symbolEnv :: SymbolEnv } deriving Show
type Compiler a = State Context a

compileClass :: Klass -> String
compileClass cls@(Klass className classVarDecs subroutineDecs) =
    let stClass = makeSymbolTableForClass cls;
        classSymbolEnv = SymbolEnv { table = stClass, outer = Nothing }
    in intercalate "" (map (compileSubroutineDec (stringOf className) classSymbolEnv) subroutineDecs)

compile :: Klass -> String
compile k = execState (compileClassM k) (Context {whileCount = 0, symbolEnv = SymbolEnv { table = M.empty, outer = Nothing }})

compileClassM :: Klass -> Compiler String
compileClassM cls@(Klass className classVarDecs subroutineDecs) =
    do
        let stClass = makeSymbolTableForClass cls
            classSymbolEnv = SymbolEnv { table = stClass, outer = Nothing }
        put $ Context {whileCount = 0, symbolEnv = classSymbolEnv}
        vms <- mapM (compileSubroutineDecM (stringOf className)) subroutineDecs
        pure $ intercalate "" vms

compileSubroutineDecM :: String -> SubroutineDec -> Compiler String
compileSubroutineDecM classStr subr@(SubroutineDec funcType returnType funcName params funcBody) = do
    ctx <- get
    let subroutineSymbols = makeSymbolTableForSubroutine subr classStr
        SubroutineBody varDecs stmts = funcBody
        se = SymbolEnv { table = subroutineSymbols, outer = Just (symbolEnv ctx) }
    put $ Context { whileCount = whileCount ctx, symbolEnv = se }
    stmtsVM <- mapM compileStmtM stmts
    pure $ "function " ++ classStr ++ "." ++ (stringOf funcName) ++ " " ++ (show $ numLocalVars funcBody) ++ "\n" ++
      intercalate "" stmtsVM
    where numLocalVars (SubroutineBody varDecs stmts) = foldr (\(VarDec _ vars) total -> length vars + total) 0 varDecs

compileSubroutineDec :: String -> SymbolEnv -> SubroutineDec -> String
compileSubroutineDec classStr classSymbols subr@(SubroutineDec funcType returnType funcName params funcBody) = 
    let subroutineSymbols = makeSymbolTableForSubroutine subr classStr;
        SubroutineBody varDecs stmts = funcBody;
        symbolEnv = SymbolEnv { table = subroutineSymbols, outer = Just classSymbols }
    in "function " ++ classStr ++ "." ++ (stringOf funcName) ++ " " ++ (show $ numLocalVars funcBody) ++ "\n" ++
       intercalate "" (map (compileStmt symbolEnv) stmts)
    where numLocalVars (SubroutineBody varDecs stmts) = foldr (\(VarDec _ vars) total -> length vars + total) 0 varDecs

compileStmt :: SymbolEnv -> Stmt -> String
compileStmt symbols (Let varTerm valExpr) = 
    let varVM = case varTerm of
                    ArrayAccess v indexExpr -> ""
                    ti@(TermIdentifier i) -> writePop (compileVal symbols ti)
    in compileExpr symbols valExpr ++ varVM
compileStmt symbols (Do subCall) = compileSubroutineCall symbols subCall 
compileStmt symbols (Return Nothing) = "pop temp 0\npush constant 0\nreturn\n"

compileStmtM :: Stmt -> Compiler String
compileStmtM (Let varTerm valExpr) = do
    varVM <- case varTerm of
                ArrayAccess v indexExpr -> pure ""
                ti@(TermIdentifier i) -> do
                    valVM <- compileValM ti
                    pure $ writePop valVM
    valVM <- compileExprM valExpr
    pure $ valVM ++ varVM
compileStmtM (Do subCall) = compileSubroutineCallM subCall 
compileStmtM (Return Nothing) = pure "pop temp 0\npush constant 0\nreturn\n"

compileVal ::  SymbolEnv -> Term -> String
compileVal symbols (TermIdentifier ti) = valToStack symbols (stringOf ti)
compileVal symbols (IntegerConstant i) = "constant " ++ show i

compileValM ::  Term -> Compiler String
compileValM (TermIdentifier ti) = valToStackM (stringOf ti)
compileValM (IntegerConstant i) = pure $ "constant " ++ show i

valToStack :: SymbolEnv -> String -> String
valToStack symbols str =
    let region = case kindOf symbols str of
                        Just kind -> case kind of
                                        "field" -> "this"
                                        "var"   -> "local"
                                        x       -> x
                        Nothing -> "error" ;
        index = case indexOf symbols str of
                    Just i -> show i
                    Nothing -> "error" 
    in region ++ " " ++ index

valToStackM :: String -> Compiler String
valToStackM str = do
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

compileExpr :: SymbolEnv -> Expr -> String
compileExpr symbols (Expr t ts) = compileTerm symbols t ++ compileArithTerms symbols ts 

compileExprM :: Expr -> Compiler String
compileExprM (Expr t ts) = (++) <$> compileTermM t <*> compileArithTermsM ts
-- compileExprM (Expr t ts) = do
--     te <- compileTermM t
--     ts <- compileArithTermsM ts
--     pure $ te ++ ts

compileTerm :: SymbolEnv -> Term -> String
compileTerm symbols ii@(IntegerConstant i) = writePush (compileVal symbols ii)
compileTerm symbols ti@(TermIdentifier i) = writePush (compileVal symbols ti)
compileTerm symbols (Paren e) = compileExpr symbols e
compileTerm symbols (TermSubroutineCall subCall) = compileSubroutineCall symbols subCall
compileTerm symbols (StringConstant s) = 
    "push constant " ++ (show $ length s) ++ "\n" ++
    "call String.new 1\n" ++
    intercalate "" (map (\c -> "push constant " ++ (show $ ord c) ++ "\ncall String.appendChar 2\n") s)

compileTermM :: Term -> Compiler String
compileTermM ii@(IntegerConstant i) = writePush <$> (compileValM ii)
compileTermM ti@(TermIdentifier i) = writePush <$> (compileValM ti)
compileTermM (Paren e) = compileExprM e
compileTermM (TermSubroutineCall subCall) = compileSubroutineCallM subCall
compileTermM (StringConstant s) = pure $
    "push constant " ++ (show $ length s) ++ "\n" ++
    "call String.new 1\n" ++
    intercalate "" (map (\c -> "push constant " ++ (show $ ord c) ++ "\ncall String.appendChar 2\n") s)

writePush :: String -> String
writePush name = "push " ++ name ++ "\n"

compileArithTerms :: SymbolEnv -> [(String, Term)] -> String
compileArithTerms symbols [] = ""
compileArithTerms symbols ((arithSymbol, term):ats) = 
    compileTerm symbols term ++
    compileArithTerms symbols ats ++
    compileArith arithSymbol

compileArithTermsM :: [(String, Term)] -> Compiler String
compileArithTermsM [] = pure ""
compileArithTermsM ((arithSymbol, term):ats) = do
    t <- compileTermM term
    a <- compileArithTermsM ats
    s <- compileArithM arithSymbol
    pure $ t ++ a ++ s

compileArith :: String -> String
compileArith "+" = "add\n"
compileArith "-" = "sub\n"
compileArith "*" = "call Math.multiply 2\n"

compileArithM :: String -> Compiler String
compileArithM "+" = pure "add\n"
compileArithM "-" = pure "sub\n"
compileArithM "*" = pure "call Math.multiply 2\n"

compileSubroutineCall symbols (SubroutineCall maybeClass func args) =
    let classStr = case maybeClass of
                        Just cls -> stringOf cls
                        Nothing -> case typeOf symbols "this" of
                                    Just cls' -> cls'
                                    Nothing -> "error";
        (argOffset, pushThis) = case maybeClass of
                        Just cls -> (0, "")
                        Nothing -> (1, "push argument 0\n")
    in  pushThis ++ 
        intercalate "" (map (compileExpr symbols) args) ++
        "call " ++ classStr ++ "." ++ stringOf func ++ " " ++ (show $ length args + argOffset) ++ "\n"

compileSubroutineCallM :: SubroutineCall -> Compiler String
compileSubroutineCallM (SubroutineCall maybeClass func args) = do
    ctx <- get
    let symbols = symbolEnv ctx
        classStr = case maybeClass of
                        Just cls -> stringOf cls
                        Nothing -> case typeOf symbols "this" of
                                    Just cls' -> cls'
                                    Nothing -> "error"
        (argOffset, pushThis) = case maybeClass of
                        Just cls -> (0, "")
                        Nothing -> (1, "push argument 0\n")
    argsVM <- mapM compileExprM args
    pure $ pushThis ++ 
        intercalate "" argsVM ++
        "call " ++ classStr ++ "." ++ stringOf func ++ " " ++ (show $ length args + argOffset) ++ "\n"
