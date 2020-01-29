module Main where

import Control.Monad
import System.Environment (getArgs)
import System.Info
import System.IO

import Parser
import Combinator
import XmlGen

main = do
    args <- getArgs
    let file = args !! 0
    content <- readFile file
    case parse (spaces >> klass) content of
        Left e -> do
            putStrLn "error! --"
            putStrLn e
        Right r -> do
            --putStrLn $ xmlGenClass r
            print r
    
px p program = case parse p program of
    Right r -> putStrLn $ xmlGenExpr r
    Left l -> putStrLn "error"