module Main where

import Data.Char
import Data.List
import System.Environment
import System.IO
import Control.Monad
import qualified Parser as P
--import qualified Code as C
import Command as C
import Code
import Text.Printf

main :: IO ()
main = do
    args <- getArgs
    let file = args !! 0
    content <- readFile file
    let commands = filter (\line -> (not . null) line && (not . P.comment) line) $ map P.trimSpace $ P.plines content
    let asm = getAsm commands 0 (fileName file)
    writeFile (changePostfix file "asm") asm
    return ()

changePostfix :: String -> String -> String
changePostfix str postfix = (++ postfix ) . reverse . dropWhile (/= '.') . reverse $ str

fileName :: String -> String
fileName str = map (\x -> if x == '/' then '.' else x) . takeWhile (/= '.') $ str
