module Code where

import Command as C
import Data.Char
import Data.List

getAsm :: [String] -> Int -> String -> String
getAsm [] n file = ""
getAsm (command:cs) n file = case C.commandType command of
    PUSH_COMMAND -> case segment command of
        "constant" -> evals ["*SP=" ++ value command]
        "local" -> evals ["*SP=LCL", "*SP=*SP+" ++ value command, "*SP=**SP"]
        "argument" -> evals ["*SP=ARG", "*SP=*SP+" ++ value command, "*SP=**SP"]
        "this" -> evals ["*SP=THIS", "*SP=*SP+" ++ value command, "*SP=**SP"]        
        "that" -> evals ["*SP=THAT", "*SP=*SP+" ++ value command, "*SP=**SP"]
        "temp" -> evals ["*SP=5", "*SP=*SP+" ++ value command, "*SP=**SP"]
        "pointer" -> evals ["*SP=3", "*SP=*SP+" ++ value command, "*SP=**SP"]
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

isPointer :: String -> Bool
isPointer val = isAlpha (head val) && all (\x -> isAlphaNum x || x == '.') val && val /= "D" && val /= "A" && val /= "M" 

isRegister :: String -> Bool
isRegister val = val == "D" || val == "A" || val == "M"

leval :: String -> String
leval expr = case expr of
    ptr | isPointer ptr -> "@" ++ ptr
    '*':pointer -> leval pointer ++ "\n" ++ "A=M"
    x -> "error " ++ x

reval :: String -> String
reval expr = case expr of
    num | all isDigit num -> "@" ++ num ++ "\n" ++ "D=A"
    ptr | isPointer ptr -> "@" ++ ptr ++ "\n" ++ "D=M"
    '*':'*':ptr | isPointer ptr -> "@" ++ ptr ++ "\nA=M\nA=M\nD=M"
    '*':ptr | isPointer ptr -> "@" ++ ptr ++ "\nA=M\nD=M"
    '*':'(':expr -> reval (init expr) ++ "\nA=D\nD=M"
    expr | exprType expr == "add" || exprType expr == "sub" ->
        let l = lexpr "+-" expr
            r = rexpr "+-" expr
        in case r of
            r | all isDigit r -> 
                reval l ++ "\n@" ++ r ++ "\nD=D" ++ binOp (exprType expr) ++ "A"

evals :: [String] -> String
evals [] = ""
evals [c] = eval c
evals (c:cs) = eval c ++ "\n" ++ evals cs

eval :: String -> String
eval expr =
    let x = lexpr "=" expr
        y = rexpr "=" expr
    in case (x,y) of
        (x,y) | isRegister x && isRegister y -> expr
        (x,y) | isRegister x && isPointer y -> "@" ++ y ++ "\n" ++ x ++ "=M"
        (x,y) | isRegister x -> reval y ++ "\n" ++ x ++ "=D"        
        (x,y) | isRegister y -> leval x ++ "\nM=" ++ y
        _  -> reval y ++ "\n" ++ leval x ++ "\nM=D"

exprType :: String -> String
exprType expr = case expr of
    _ | isInfixOf "+" expr -> "add"
    _ | isInfixOf "-" expr -> "sub"
    _                      -> "other"

binOp :: String -> String
binOp "add" = "+"
binOp "sub" = "-"

lexpr :: String -> String -> String
lexpr pattern expr = takeWhile (noneOf pattern) expr

rexpr :: String -> String -> String
rexpr pattern expr = dropWhile (anyOf pattern) . dropWhile (noneOf pattern) $ expr

noneOf :: String -> Char -> Bool
noneOf [s]    c = c /= s
noneOf (s:rs) c = if c /= s then noneOf rs c else False

anyOf :: String -> Char -> Bool
anyOf [s]    c = c == s
anyOf (s:rs) c = if c == s then True else anyOf rs c
