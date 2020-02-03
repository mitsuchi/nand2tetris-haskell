module SymbolTable where

import qualified Data.Map as M

data SymbolEnv = SymbolEnv {table :: SymbolTable, outer :: Maybe SymbolEnv} deriving Show
type SymbolTable = M.Map String SymbolRow
data SymbolRow = SymbolRow {typeName :: String, kind :: String, index :: Int} deriving Show

varCount :: SymbolTable -> String -> Int
varCount st k = M.foldr (\a b -> if kind a == k then b+1 else b) 0 st

kindOf :: SymbolEnv -> String -> Maybe String
kindOf se name = do 
    row <- lookupEnv name se
    return $ (kind row)

typeOf :: SymbolEnv -> String -> Maybe String
typeOf se name = do 
    row <- lookupEnv name se
    return $ (typeName row)

indexOf :: SymbolEnv -> String -> Maybe Int
indexOf se name = do 
    row <- lookupEnv name se
    return $ (index row)

lookupEnv :: String -> SymbolEnv -> Maybe SymbolRow
lookupEnv name se = case M.lookup name (table se) of
    Just row -> Just row
    Nothing -> case outer se of
        Just outerSymbolEnv -> lookupEnv name outerSymbolEnv
        Nothing -> Nothing

define :: SymbolTable -> String -> String -> String -> Int -> SymbolTable
define st name typeName kind index = 
    M.insert name (SymbolRow {typeName=typeName, kind=kind, index=index}) st

