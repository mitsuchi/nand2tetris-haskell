module Main where

import Data.Char
import Data.List
import System.Directory
import System.Environment
import System.IO
import Control.Monad
import qualified Parser as P
import Command as C
import Code
import Text.Printf

main :: IO ()
main = do
    args <- getArgs
    let file = args !! 0
    if isSuffixOf ".vm" file
    then do
        asm <- compileFromFile file
        writeFile (changePostfix file "asm") (bootStrap ++ "\n" ++ asm)
        --writeFile (changePostfix file "asm") (eval "SP=256" ++ "\n" ++ asm)
    else do 
        asm <- compileFromDirectory file
        writeFile (file ++ "/" ++ (dirName file) ++ ".asm") (bootStrap ++ "\n" ++ asm)

compileFromFile :: String -> IO String
compileFromFile file = do
        content <- readFile file
        return $ compileFile content file

compileFromDirectory :: String -> IO String
compileFromDirectory dir = do
        files <- listDirectory dir
        asms <- forM (filter (isSuffixOf ".vm") files) $ \file -> compileFromFile $ dir ++ "/" ++ file
        return $ intercalate "\n" asms

compileFile :: String -> String -> String
compileFile content file = 
    let commands = filter (\line -> (not . null) line && (not . P.comment) line) $ map P.trimSpace $ P.plines content
    in getAsm commands 0 (fileName file)

changePostfix :: String -> String -> String
changePostfix str postfix = (++ postfix ) . reverse . dropWhile (/= '.') . reverse $ str

fileName :: String -> String
fileName str = map (\x -> if x == '/' then '.' else x) . takeWhile (/= '.') $ str

dirName :: String -> String
dirName str = reverse . dropWhile (== '/') . takeWhile (/= '/') . reverse $ str