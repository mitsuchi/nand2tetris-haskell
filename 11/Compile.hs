module Compile where

import AST
import Combinator
import Data.Char
import Data.List
import qualified Data.Map as M
import SymbolTable

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

compileClass :: Klass -> String
compileClass cls@(Klass className classVarDecs subroutineDecs) =
    let stClass = makeSymbolTableForClass cls;
        classSymbolEnv = SymbolEnv { table = stClass, outer = Nothing }
    in intercalate "" (map (compileSubroutineDec (stringOf className) classSymbolEnv) subroutineDecs)

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

compileVal ::  SymbolEnv -> Term -> String
compileVal symbols (TermIdentifier ti) = valToStack symbols (stringOf ti)
compileVal symbols (IntegerConstant i) = "constant " ++ show i

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

writePop :: String -> String
writePop name = "pop " ++ name ++ "\n"

compileExpr :: SymbolEnv -> Expr -> String
compileExpr symbols (Expr t ts) = compileTerm symbols t ++ compileArithTerms symbols ts 

compileTerm :: SymbolEnv -> Term -> String
compileTerm symbols ii@(IntegerConstant i) = writePush (compileVal symbols ii)
compileTerm symbols ti@(TermIdentifier i) = writePush (compileVal symbols ti)
compileTerm symbols (Paren e) = compileExpr symbols e
compileTerm symbols (TermSubroutineCall subCall) = compileSubroutineCall symbols subCall
compileTerm symbols (StringConstant s) = 
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

compileArith :: String -> String
compileArith "+" = "add\n"
compileArith "-" = "sub\n"
compileArith "*" = "call Math.multiply 2\n"

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
