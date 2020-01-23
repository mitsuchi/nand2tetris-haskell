module CodeGen where

import Combinator

-- for mac x86_64
codeGenAll expr os@"darwin" arch@"x86_64" = 
    ".intel_syntax noprefix\n" ++
    "    .global _main\n" ++
    "_main:\n" ++
    (codeGen expr os arch) ++
    "     pop rax\n" ++
    "     ret\n"

-- for arm64ma
codeGenAll expr os arch = 
    "    .global main\n" ++
    "main:\n" ++
    (codeGen expr os arch) ++
    "     ldr w0, [sp], #16\n" ++
    "     ret\n"

-- for x86_64
codeGen (IntLit i) "darwin" "x86_64" = 
    "     push " ++ show i ++ "\n"

codeGen (BinOp op e1 e2) os@"darwin" arch@"x86_64" =
    codeGen e1 os arch ++ codeGen e2 os arch ++
    "     pop rdi\n" ++
    "     pop rax\n" ++
    "     " ++ operandForBinOp op os arch ++ "\n" ++
    "     push rax\n"

-- for arm64
codeGen (IntLit i) os arch = 
    "     mov w0, " ++ show i ++ "\n" ++
    "     str w0, [sp, #-16]!\n"

codeGen (BinOp op e1 e2) os arch =
    codeGen e1 os arch ++ codeGen e2 os arch ++
    "     ldr w0, [sp], #16\n" ++
    "     ldr w1, [sp], #16\n" ++
    "     " ++ operandForBinOp op os arch ++ " w0, w1, w0\n" ++
    "     str w0, [sp, #-16]!\n"

-- for mac x86_64    
operandForBinOp op "darwin" "x86_64" = case op of
    "+" -> "add rax, rdi"
    "-" -> "sub rax, rdi"
    "*" -> "imul rax, rdi"
    "/" -> "cqo\n" ++ "     idiv rdi"
    "==" -> "cmp rax, rdi\n" ++
            "     sete al\n" ++
            "     movzx rax, al"
    "!=" -> "cmp rax, rdi\n" ++
            "     setne al\n" ++
            "     movzx rax, al"
    "<"  -> "cmp rax, rdi\n" ++
            "     setl al\n" ++
            "     movzx rax, al"
    "<=" -> "cmp rax, rdi\n" ++
            "     setle al\n" ++
            "     movzx rax, al"


-- for arm64
operandForBinOp op os arch = case op of
    "+" -> "add"
    "-" -> "sub"
    "*" -> "mul"
    "/" -> "sdiv"