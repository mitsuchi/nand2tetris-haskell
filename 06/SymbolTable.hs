module SymbolTable where

import qualified Data.Map.Strict as M

type SymbolMap = M.Map String Int

addEntry :: SymbolMap -> String -> Int -> SymbolMap
addEntry smap symbol address = M.insert symbol address smap

contains :: SymbolMap -> String -> Bool
contains smap symbol = M.member symbol smap

getAddress :: SymbolMap -> String -> Maybe Int
getAddress smap symbol = M.lookup symbol smap