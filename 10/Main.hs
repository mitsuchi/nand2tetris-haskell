module Main where

import Control.Monad
import System.Environment (getArgs)
import System.Info
import System.IO

import Parser
import Combinator
--import XmlGen

main = do
    args <- getArgs
    let file = args !! 0
    content <- readFile file
    case parse klass content of
        Left  e -> print e
        --Right r -> putStrLn $ codeGenAll r os arch
        Right r -> print r
    
    