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
        "local" -> evals ["*SP=LCL+" ++ value command, "--SP", "D=*SP", "++SP", "**SP=D"]
        "argument" -> evals ["*SP=ARG+" ++ value command, "--SP", "D=*SP", "++SP", "**SP=D"]
        "this" -> evals ["*SP=THIS+" ++ value command, "--SP", "D=*SP", "++SP", "**SP=D"]
        "that" -> evals ["*SP=THAT+" ++ value command, "--SP", "D=*SP", "++SP", "**SP=D"]
        "temp" -> evals ["*SP=5+" ++ value command, "--SP", "D=*SP", "++SP", "**SP=D"]
        "pointer" -> evals ["*SP=3+" ++ value command, "--SP", "D=*SP", "++SP", "**SP=D"]
        "static" -> "@SP\nM=M-1\nA=M\nD=M\n@" ++ file ++ "." ++ value command ++ "\nM=D\n" ++ inc "SP"
      ++ "\n" ++ decr "SP"
    CALC_COMMAND -> case operand command of
        "add" -> evals ["--SP", "D=*SP", "--SP", "*SP=*SP+D", "++SP"]        
        "sub" -> evals ["--SP", "D=*SP", "--SP", "*SP=*SP-D", "++SP"]
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
    LABEL_COMMAND -> "(" ++ label command ++ ")"
    IF_GOTO_COMMAND -> evals ["SP=SP-1","D=*SP"] ++ "\n@" ++ file ++ "." ++ label command ++ "\nD;JNE"
    GOTO_COMMAND -> "@" ++ label command ++ "\n0;JMP"
    FUNCTION_COMMAND -> writeFunction (function command) (numLocalVar command)
    RETURN_COMMAND -> writeReturn
    CALL_COMMAND -> writeCall (function command) (numArg command) n
   ++ "\n" ++ getAsm cs (n+1) file

writeReturn :: String
writeReturn = evals ["R13=LCL","RET=*(R13-5)","--SP","*ARG=*SP","SP=ARG+1","THAT=*(R13-1)",
    "THIS=*(R13-2)","ARG=*(R13-3)","LCL=*(R13-4)"] ++ "\n@RET\n0;JMP"

writeFunction :: String -> Int -> String
writeFunction name n = "(" ++ name ++ ")\n" ++ intercalate "\n" (map (\x -> evals ["*SP=0","++SP"]) [0..(n-1)])

writeCall :: String -> Int -> Int -> String
writeCall func n uniqNum = evals ["*SP=return.address." ++ (show uniqNum), "++SP", "*SP=LCL", "++SP",
   "*SP=ARG", "++SP", "*SP=THIS", "++SP", "*SP=THAT", "++SP",
   "ARG=SP-" ++ show (n+5), "LCL=SP"] ++ writeGoto ("return.address." ++ (show uniqNum))

writeGoto :: String -> String
writeGoto gotoLabel = "@" ++ gotoLabel ++ "\n0;JMP"

inc :: String -> String
inc symbol = "@" ++ symbol ++ "\nM=M+1"

decr :: String -> String
decr symbol = "@" ++ symbol ++ "\nM=M-1"

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
            _ -> "error :expr=" ++ expr ++ ", lexpr=" ++ l ++ ", rexpr=" ++ r

evals :: [String] -> String
evals [] = ""
evals [c] = eval c
evals (c:cs) = eval c ++ "\n" ++ evals cs

eval :: String -> String
eval expr =
    let x = lexpr "=" expr
        y = rexpr "=" expr
    in case (x,y) of
        ('+':'+':pointer, y) -> "@" ++ pointer ++ "\nM=M+1"
        ('-':'-':pointer, y) -> "@" ++ pointer ++ "\nM=M-1"
        ('*':ptr, y) | y == "*" ++ ptr ++ "+D" -> "@" ++ ptr ++ "\nA=M\nM=M+D"
        ('*':ptr, y) | y == "*" ++ ptr ++ "-D" -> "@" ++ ptr ++ "\nA=M\nM=M-D"
        (x,y) | isRegister x && isRegister y -> expr
        (x,y) | isRegister x && isPointer y -> "@" ++ y ++ "\n" ++ x ++ "=M"
        ("D",y) -> reval y
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
