module Main where

import Control.Monad
import Data.List
import System.Directory
import System.Environment (getArgs)
import System.Info
import System.IO

import Parser
import Combinator
import Compile
import XmlGen

main = do
    args <- getArgs
    let file = args !! 0
    if isSuffixOf ".jack" file
    then compileFromFile file
    else compileFromDirectory file

compileFromFile file = do
    content <- readFile file
    case parse (spaces >> klass) content of
        Left e -> do
            putStrLn "error! --"
            putStrLn e
        Right r -> do
            writeFile (changePostfix file "vm") (compile r)

compileFromDirectory dir = do
    files <- listDirectory dir
    forM_ (filter (isSuffixOf ".jack") files) $ \file -> compileFromFile $ dir ++ "/" ++ file
            
pexpr program = case parse expr program of
    Right r -> putStr $ xmlGenExpr r
    Left l -> putStrLn "error"

pclass program = case parse klass program of
    Right r -> putStr $ xmlGenClass r
    Left l -> putStrLn "error"

stclass program = case parse klass program of
        Right r -> print $ makeSymbolTableForClass r
        Left l -> putStrLn "error"

stsubroutine program = case parse subroutineDec program of
        Right r -> print $ makeSymbolTableForSubroutine r "DummyClass"
        Left l -> putStrLn "error"        

mclass program = case parse klass program of
    Right r -> putStr $ compile r
    Left l -> putStrLn $ "error: " ++ l

cf file = do
    content <- readFile file
    case parse (spaces >> klass) content of
        Right r -> putStr $ compile r
        Left l -> putStrLn $ "error: " ++ l

xf file = do
    content <- readFile file
    case parse (spaces >> klass) content of
        Right r -> putStr $ xmlGenClass r
        Left l -> putStrLn $ "error: " ++ l

changePostfix :: String -> String -> String
changePostfix str postfix = (++ postfix ) . reverse . dropWhile (/= '.') . reverse $ str