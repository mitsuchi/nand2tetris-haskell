module Main where

import Control.Monad
import System.Environment (getArgs)
import System.Info

import Parser
import Combinator
import CodeGen

main = do
    args <- getArgs
    case parse program (args !! 0) of
        Left  e -> putStrLn e
        Right r -> putStrLn $ codeGenAll r os arch
