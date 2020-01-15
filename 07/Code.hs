module Code where

import Command as C

getAsm :: [String] -> Int -> String -> String
getAsm [] n file = ""
getAsm (command:cs) n file = case C.commandType command of
    PUSH_COMMAND -> case segment command of
        "constant" -> "@" ++ value command ++ "\nD=A\n@SP\nA=M\nM=D"
        "local" -> "@LCL\nD=M\n@SP\nA=M\nM=D\n@" ++ value command ++ "\nD=A\n@SP\nA=M\nM=D+M\nA=M\nD=M\n@SP\nA=M\nM=D"
        "argument" -> "@ARG\nD=M\n@SP\nA=M\nM=D\n@" ++ value command ++ "\nD=A\n@SP\nA=M\nM=D+M\nA=M\nD=M\n@SP\nA=M\nM=D"
        "this" -> "@THIS\nD=M\n@SP\nA=M\nM=D\n@" ++ value command ++ "\nD=A\n@SP\nA=M\nM=D+M\nA=M\nD=M\n@SP\nA=M\nM=D"
        "that" -> "@THAT\nD=M\n@SP\nA=M\nM=D\n@" ++ value command ++ "\nD=A\n@SP\nA=M\nM=D+M\nA=M\nD=M\n@SP\nA=M\nM=D"
        "temp" -> "@5\nD=A\n@SP\nA=M\nM=D\n@" ++ value command ++ "\nD=A\n@SP\nA=M\nM=D+M\nA=M\nD=M\n@SP\nA=M\nM=D"
        "pointer" -> "@3\nD=A\n@SP\nA=M\nM=D\n@" ++ value command ++ "\nD=A\n@SP\nA=M\nM=D+M\nA=M\nD=M\n@SP\nA=M\nM=D"
        "static" -> "@" ++ file ++ "." ++ value command ++ "\nD=M\n@SP\nA=M\nM=D"
      ++ inc "SP"
    POP_COMMAND -> case segment command of
        "local" -> "@LCL\nD=M\n@SP\nA=M\nM=D\n@" ++ value command ++ "\nD=A\n@SP\nA=M\nM=D+M\n@SP\nM=M-1\nA=M\nD=M\n@SP\nM=M+1\nA=M\nA=M\nM=D"
        "argument" -> "@ARG\nD=M\n@SP\nA=M\nM=D\n@" ++ value command ++ "\nD=A\n@SP\nA=M\nM=D+M\n@SP\nM=M-1\nA=M\nD=M\n@SP\nM=M+1\nA=M\nA=M\nM=D"
        "this" -> "@THIS\nD=M\n@SP\nA=M\nM=D\n@" ++ value command ++ "\nD=A\n@SP\nA=M\nM=D+M\n@SP\nM=M-1\nA=M\nD=M\n@SP\nM=M+1\nA=M\nA=M\nM=D"
        "that" -> "@THAT\nD=M\n@SP\nA=M\nM=D\n@" ++ value command ++ "\nD=A\n@SP\nA=M\nM=D+M\n@SP\nM=M-1\nA=M\nD=M\n@SP\nM=M+1\nA=M\nA=M\nM=D"
        "temp" -> "@5\nD=A\n@SP\nA=M\nM=D\n@" ++ value command ++ "\nD=A\n@SP\nA=M\nM=D+M\n@SP\nM=M-1\nA=M\nD=M\n@SP\nM=M+1\nA=M\nA=M\nM=D"
        "pointer" -> "@3\nD=A\n@SP\nA=M\nM=D\n@" ++ value command ++ "\nD=A\n@SP\nA=M\nM=D+M\n@SP\nM=M-1\nA=M\nD=M\n@SP\nM=M+1\nA=M\nA=M\nM=D"
        "static" -> "@SP\nM=M-1\nA=M\nD=M\n@" ++ file ++ "." ++ value command ++ "\nM=D" ++ inc "SP"
      ++ decr "SP"
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
inc symbol = "\n@" ++ symbol ++ "\nM=M+1\n"

decr :: String -> String
decr symbol = "\n@" ++ symbol ++ "\nM=M-1\n"