module Code where

import Command as C
import Data.Char

getAsm :: [String] -> Int -> String -> String
getAsm [] n file = ""
getAsm (command:cs) n file = case C.commandType command of
    PUSH_COMMAND -> case segment command of
        "constant" -> evals ["D=" ++ value command, "*SP=D"]
        "local" -> eval "D=LCL" ++ "\n" ++ push (value command)
        "argument" -> eval "D=ARG" ++ "\n" ++ push (value command)
        "this" -> eval "D=THIS" ++ "\n" ++ push (value command)
        "that" -> eval "D=THAT" ++ "\n" ++ push (value command)
        "temp" -> eval "D=5" ++ "\n" ++ push (value command)
        "pointer" -> eval "D=3" ++ "\n" ++ push (value command)
        "static" -> eval ("D=" ++ (file ++ "." ++ value command)) ++ "\n@SP\nA=M\nM=D"
      ++ "\n" ++ inc "SP"
    POP_COMMAND -> case segment command of
        "local" -> "@LCL\nD=M\n@SP\nA=M\nM=D\n@" ++ value command ++ "\nD=A\n@SP\nA=M\nM=D+M\n@SP\nM=M-1\nA=M\nD=M\n@SP\nM=M+1\nA=M\nA=M\nM=D"
        "argument" -> "@ARG\nD=M\n@SP\nA=M\nM=D\n@" ++ value command ++ "\nD=A\n@SP\nA=M\nM=D+M\n@SP\nM=M-1\nA=M\nD=M\n@SP\nM=M+1\nA=M\nA=M\nM=D"
        "this" -> "@THIS\nD=M\n@SP\nA=M\nM=D\n@" ++ value command ++ "\nD=A\n@SP\nA=M\nM=D+M\n@SP\nM=M-1\nA=M\nD=M\n@SP\nM=M+1\nA=M\nA=M\nM=D"
        "that" -> "@THAT\nD=M\n@SP\nA=M\nM=D\n@" ++ value command ++ "\nD=A\n@SP\nA=M\nM=D+M\n@SP\nM=M-1\nA=M\nD=M\n@SP\nM=M+1\nA=M\nA=M\nM=D"
        "temp" -> "@5\nD=A\n@SP\nA=M\nM=D\n@" ++ value command ++ "\nD=A\n@SP\nA=M\nM=D+M\n@SP\nM=M-1\nA=M\nD=M\n@SP\nM=M+1\nA=M\nA=M\nM=D"
        "pointer" -> "@3\nD=A\n@SP\nA=M\nM=D\n@" ++ value command ++ "\nD=A\n@SP\nA=M\nM=D+M\n@SP\nM=M-1\nA=M\nD=M\n@SP\nM=M+1\nA=M\nA=M\nM=D"
        "static" -> "@SP\nM=M-1\nA=M\nD=M\n@" ++ file ++ "." ++ value command ++ "\nM=D\n" ++ inc "SP"
      ++ "\n" ++ decr "SP"
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

inc :: String -> String
inc symbol = "@" ++ symbol ++ "\nM=M+1"

decr :: String -> String
decr symbol = "@" ++ symbol ++ "\nM=M-1"

setD :: String -> String -> String
setD "toAddrOf" symbol = "@" ++ symbol ++ "\nD=M"
setD "to" val = "@" ++ val ++ "\nD=A"

setRamByDAtPointer :: String -> String
setRamByDAtPointer symbol = "@" ++ symbol ++ "\nA=M\nM=D"

push :: String -> String
push val = setRamByDAtPointer "SP" ++ "\n" ++ setD "to" val ++ "\n@SP\nA=M\nM=D+M\nA=M\nD=M\n@SP\nA=M\nM=D"

set :: String -> String
set "hoge" = "moge"

evals :: [String] -> String
evals [] = ""
evals (c:cs) = eval c ++ "\n" ++ evals cs

eval :: String -> String
eval command = 
    let left = takeWhile (/= '=') command
        right = dropWhile (== '=') . dropWhile (/= '=') $ command 
    in case (left, right) of
        (reg, val)     | all isDigit val    -> "@" ++ val ++ "\n" ++ reg ++ "=A"
        (reg, pointer) | isPointer pointer  -> "@" ++ pointer ++ "\n" ++ reg ++ "=M"
        ('*':pointer, reg) | isRegister reg -> "@" ++ pointer ++ "\nA=M\nM=D"

isPointer :: String -> Bool
isPointer val = all (\x -> isAlpha x || x == '.') val && val /= "D" && val /= "A" && val /= "M"

isRegister :: String -> Bool
isRegister val = val == "D" || val == "A"