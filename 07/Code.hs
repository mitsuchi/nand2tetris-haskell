module Code where

import Command as C
import Data.Char
import Data.List

getAsm :: [String] -> Int -> String -> String
getAsm [] n file = ""
getAsm (command:cs) n file = case C.commandType command of
    PUSH_COMMAND -> case segment command of
        "constant" -> evals ["*SP=" ++ value command]
        "local" -> evals ["*SP=LCL", "*SP+=" ++ value command, "*SP=**SP"]
        "argument" -> evals ["*SP=ARG", "*SP+=" ++ value command, "*SP=**SP"]
        "this" -> evals ["*SP=THIS", "*SP+=" ++ value command, "*SP=**SP"]        
        "that" -> evals ["*SP=THAT", "*SP+=" ++ value command, "*SP=**SP"]
        "temp" -> evals ["*SP=5", "*SP+=" ++ value command, "*SP=**SP"]
        "pointer" -> evals ["*SP=3", "*SP+=" ++ value command, "*SP=**SP"]
        --"static" -> eval ("D=" ++ (file ++ "." ++ value command)) ++ "\n@SP\nA=M\nM=D"
        "static" -> evals ["*SP=" ++ (file ++ "." ++ value command)]
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

push :: String -> String
push val = evals ["*SP=D", "D=" ++ val, "A=SP"] ++ "\nM=D+M\nA=M\nD=M\n@SP\nA=M\nM=D"

evals :: [String] -> String
evals [] = ""
evals [c] = eval c
evals (c:cs) = eval c ++ "\n" ++ evals cs

eval :: String -> String
eval command = case subCommandType command of
    "assign" -> 
        let left = takeWhile (/= '=') command
            right = dropWhile (== '=') . dropWhile (/= '=') $ command 
        in case (left, right) of
            ('*':p1, '*':'*':p2) | isPointer p1 && isPointer p2 -> evals ["D=**" ++ p2, "*" ++ p1 ++ "=D"]
            ('*':pointer, const) | all isDigit const -> evals ["D=" ++ const, "*" ++ pointer ++ "=D"]
            ('*':pointer, reg) | isRegister reg -> "@" ++ pointer ++ "\nA=M\nM=D"
            ('*':pointer1, pointer2) | isPointer pointer2 -> evals ["D=" ++ pointer2, "*" ++ pointer1 ++ "=D"]
            (reg, '*':'*':pointer) | isPointer pointer  -> eval ( "A=*" ++ pointer) ++ "\nD=M"
            (reg, '*':pointer) | isPointer pointer  -> "@" ++ pointer ++ "\nA=M\n" ++ reg ++ "=M"
            (reg, pointer) | isPointer pointer  -> "@" ++ pointer ++ "\n" ++ reg ++ "=M"
            (reg, val)     | all isDigit val    -> "@" ++ val ++ "\n" ++ reg ++ "=A"
            _              -> left ++ "|" ++ right
    "add" ->
        let left = takeWhile (/= '+') command
            right = dropWhile (== '=') . dropWhile (/= '=') $ command 
        in case (left, right) of
            ('*':pointer, val) -> evals ["D=" ++ val] ++ "\n@" ++ pointer ++ "\n" ++ "A=M\nM=D+M"
            (pointer, val)     -> evals ["D=" ++ val] ++ "\n@" ++ pointer ++ "\n" ++ "M=D+M"            
    "other" -> command

isPointer :: String -> Bool
isPointer val = isAlpha (head val) && all (\x -> isAlphaNum x || x == '.' || x == '-') val && val /= "D" && val /= "A" && val /= "M" 

isRegister :: String -> Bool
isRegister val = val == "D" || val == "A" 

subCommandType :: String -> String
subCommandType command = case command of
    _ | isInfixOf "+=" command -> "add"
    _ | isInfixOf "-=" command -> "sub"
    _ | isInfixOf "=" command -> "assign"
    _                 -> "other"
