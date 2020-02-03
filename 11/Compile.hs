module Compile where

import AST
import Combinator
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
    in foldr (\v st -> define st (stringOf v) t k (varCount st k)) table vars

makeSymbolTableForSubroutine :: SubroutineDec -> SymbolTable
makeSymbolTableForSubroutine (SubroutineDec _ _ _ params subroutineBody) =
    let SubroutineBody varDecs stmts = subroutineBody;
        st = makeSymbolTableForSubroutine' varDecs M.empty
    in makeSymbolTableForSubroutineArgs params st

makeSymbolTableForSubroutine' :: [VarDec] -> SymbolTable -> SymbolTable
makeSymbolTableForSubroutine' [] table = table
makeSymbolTableForSubroutine' (dec:decs) table = 
    makeSymbolTableForSubroutine' decs (addVarDecToTable dec table)    

addVarDecToTable :: VarDec -> SymbolTable -> SymbolTable
addVarDecToTable (VarDec typeName vars) table =
    let t = stringOf typeName;
        k = "var"
    in foldr (\v st -> define st (stringOf v) t k (varCount st k)) table vars

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
    let stClass = makeSymbolTableForClass cls
    in intercalate "" (map (compileSubroutineDec (stringOf className) stClass) subroutineDecs)

compileSubroutineDec :: String -> SymbolTable -> SubroutineDec -> String
compileSubroutineDec classStr classSymbols subr@(SubroutineDec funcType returnType funcName params funcBody) = 
    let subroutineSymbols = makeSymbolTableForSubroutine subr
    in "function " ++ classStr ++ "." ++ (stringOf funcName) ++ " " ++ (show $ numLocalVars funcBody) ++ "\n"
    where numLocalVars (SubroutineBody varDecs stmts) = length varDecs