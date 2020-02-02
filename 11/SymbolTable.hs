module SymbolTable where

import qualified Data.Map as M

data SymbolEnv = SymbolEnv {table :: SymbolTable, outer :: Maybe SymbolEnv} deriving Show
type SymbolTable = M.Map String SymbolRow
data SymbolRow = SymbolRow {typeName :: String, kind :: String, index :: Int} deriving Show

varCount :: SymbolTable -> String -> Int
varCount st k = M.foldr (\a b -> if kind a == k then b+1 else b) 0 st

kindOf :: SymbolTable -> String -> Maybe String
kindOf st name = do 
    row <- M.lookup name st
    return $ (kind row)

typeOf :: SymbolTable -> String -> Maybe String
typeOf st name = do 
    row <- M.lookup name st
    return $ (typeName row)

indexOf :: SymbolTable -> String -> Maybe Int
indexOf st name = do 
    row <- M.lookup name st
    return $ (index row)

define :: SymbolTable -> String -> String -> String -> Int -> SymbolTable
define st name typeName kind index = 
    M.insert name (SymbolRow {typeName=typeName, kind=kind, index=index}) st

