module Main where

import Data.Char
import Data.List
import System.Environment
import System.IO
import Control.Monad
import qualified Parser as P
--import qualified Code as C
import Command as C
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

getAsm :: [String] -> Int -> String -> String
getAsm [] n file = ""
getAsm (command:cs) n file = case C.commandType command of
    PUSH_COMMAND -> case segment command of
        "constant" -> "@" ++ value command ++ "\nD=A\n@SP\nA=M\nM=D\n@SP\nM=M+1"
        "local" -> "@LCL\nD=M\n@SP\nA=M\nM=D\n@" ++ value command ++ "\nD=A\n@SP\nA=M\nM=D+M\nA=M\nD=M\n@SP\nA=M\nM=D\n@SP\nM=M+1"
        "argument" -> "@ARG\nD=M\n@SP\nA=M\nM=D\n@" ++ value command ++ "\nD=A\n@SP\nA=M\nM=D+M\nA=M\nD=M\n@SP\nA=M\nM=D\n@SP\nM=M+1"        
        "this" -> "@THIS\nD=M\n@SP\nA=M\nM=D\n@" ++ value command ++ "\nD=A\n@SP\nA=M\nM=D+M\nA=M\nD=M\n@SP\nA=M\nM=D\n@SP\nM=M+1"        
        "that" -> "@THAT\nD=M\n@SP\nA=M\nM=D\n@" ++ value command ++ "\nD=A\n@SP\nA=M\nM=D+M\nA=M\nD=M\n@SP\nA=M\nM=D\n@SP\nM=M+1"        
        "temp" -> "@5\nD=A\n@SP\nA=M\nM=D\n@" ++ value command ++ "\nD=A\n@SP\nA=M\nM=D+M\nA=M\nD=M\n@SP\nA=M\nM=D\n@SP\nM=M+1"        
        "pointer" -> "@3\nD=A\n@SP\nA=M\nM=D\n@" ++ value command ++ "\nD=A\n@SP\nA=M\nM=D+M\nA=M\nD=M\n@SP\nA=M\nM=D\n@SP\nM=M+1"        
        "static" -> "@" ++ file ++ "." ++ value command ++ "\nD=M\n@SP\nA=M\nM=D\n@SP\nM=M+1"
    POP_COMMAND -> case segment command of
        "local" -> "@LCL\nD=M\n@SP\nA=M\nM=D\n@" ++ value command ++ "\nD=A\n@SP\nA=M\nM=D+M\n@SP\nM=M-1\nA=M\nD=M\n@SP\nM=M+1\nA=M\nA=M\nM=D\n@SP\nM=M-1"
        "argument" -> "@ARG\nD=M\n@SP\nA=M\nM=D\n@" ++ value command ++ "\nD=A\n@SP\nA=M\nM=D+M\n@SP\nM=M-1\nA=M\nD=M\n@SP\nM=M+1\nA=M\nA=M\nM=D\n@SP\nM=M-1"
        "this" -> "@THIS\nD=M\n@SP\nA=M\nM=D\n@" ++ value command ++ "\nD=A\n@SP\nA=M\nM=D+M\n@SP\nM=M-1\nA=M\nD=M\n@SP\nM=M+1\nA=M\nA=M\nM=D\n@SP\nM=M-1"
        "that" -> "@THAT\nD=M\n@SP\nA=M\nM=D\n@" ++ value command ++ "\nD=A\n@SP\nA=M\nM=D+M\n@SP\nM=M-1\nA=M\nD=M\n@SP\nM=M+1\nA=M\nA=M\nM=D\n@SP\nM=M-1"
        "temp" -> "@5\nD=A\n@SP\nA=M\nM=D\n@" ++ value command ++ "\nD=A\n@SP\nA=M\nM=D+M\n@SP\nM=M-1\nA=M\nD=M\n@SP\nM=M+1\nA=M\nA=M\nM=D\n@SP\nM=M-1"
        "pointer" -> "@3\nD=A\n@SP\nA=M\nM=D\n@" ++ value command ++ "\nD=A\n@SP\nA=M\nM=D+M\n@SP\nM=M-1\nA=M\nD=M\n@SP\nM=M+1\nA=M\nA=M\nM=D\n@SP\nM=M-1"
        "static" -> "@SP\nM=M-1\nA=M\nD=M\n@" ++ file ++ "." ++ value command ++ "\nM=D"
    CALC_COMMAND -> case operand command of
        "add" -> "@SP\nM=M-1\n@SP\nA=M\nA=M\nD=A\n@SP\nM=M-1\n@SP\nA=M\nM=D+M\n@SP\nM=M+1"
        "sub" -> "@SP\nM=M-1\n@SP\nA=M\nA=M\nD=A\n@SP\nM=M-1\n@SP\nA=M\nM=M-D\n@SP\nM=M+1"
        "neg" -> "@SP\nM=M-1\nA=M\nM=-M\n@SP\nM=M+1"
        "eq"  -> "@SP\nM=M-1\n@SP\nA=M\nA=M\nD=A\n@SP\nM=M-1\n@SP\nA=M\nM=M-D\nD=M\n" ++ 
            "@if-then" ++ show n ++ "\nD;JEQ\n@SP\nA=M\nM=0\n@if-end" ++ show n ++ "\n0;JMP\n" ++
            "(if-then" ++ show n ++ ")\n@SP\nA=M\nM=-1\n(if-end" ++ show n ++ ")\n@SP\nM=M+1"
        "gt"  -> "@SP\nM=M-1\n@SP\nA=M\nA=M\nD=A\n@SP\nM=M-1\n@SP\nA=M\nM=M-D\nD=M\n" ++ 
            "@if-then" ++ show n ++ "\nD;JGT\n@SP\nA=M\nM=0\n@if-end" ++ show n ++ "\n0;JMP\n" ++
            "(if-then" ++ show n ++ ")\n@SP\nA=M\nM=-1\n(if-end" ++ show n ++ ")\n@SP\nM=M+1"
        "lt"  -> "@SP\nM=M-1\n@SP\nA=M\nA=M\nD=A\n@SP\nM=M-1\n@SP\nA=M\nM=M-D\nD=M\n" ++ 
             "@if-then" ++ show n ++ "\nD;JLT\n@SP\nA=M\nM=0\n@if-end" ++ show n ++ "\n0;JMP\n" ++
              "(if-then" ++ show n ++ ")\n@SP\nA=M\nM=-1\n(if-end" ++ show n ++ ")\n@SP\nM=M+1"                
        "and" -> "@SP\nM=M-1\n@SP\nA=M\nA=M\nD=A\n@SP\nM=M-1\n@SP\nA=M\nM=D&M\n@SP\nM=M+1"
        "or" -> "@SP\nM=M-1\n@SP\nA=M\nA=M\nD=A\n@SP\nM=M-1\n@SP\nA=M\nM=D|M\n@SP\nM=M+1"
        "not" -> "@SP\nM=M-1\nA=M\nM=!M\n@SP\nM=M+1"
        _     -> "1" ++ operand command ++ "2"
  ++ "\n" ++ getAsm cs (n+1) file

changePostfix :: String -> String -> String
changePostfix str postfix = (++ postfix ) . reverse . dropWhile (/= '.') . reverse $ str

fileName :: String -> String
fileName str = map (\x -> if x == '/' then '-' else x) . takeWhile (/= '.') $ str