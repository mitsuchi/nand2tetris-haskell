# 次第に抽象化して行くのがいい

7+8

-> 

push 7
push 8
add

-> 
// init
ROM[SP]=10
// push 7
ROM[ROM[SP]]=7
ROM[SP]=ROM[SP]+1
// push 8
ROM[ROM[SP]]=8
ROM[SP]=ROM[SP]+1
// add
pop to D
ROM[SP]=ROM[SP]-1
D=ROM[ROM[SP]]
ROM[SP]=ROM[SP]-1
ROM[ROM[SP]]=ROM[ROM[SP]]+D
ROM[SP]=ROM[SP]-1
->

# ROM[SP]=10
D=10
A=SP
M=D

## ROM[SP]=10
A=10
D=A
A=SP
M=D

## ROM[X]=Y
A=Y
D=A
A=X
M=D

->
@A
D=A
@X
M=D

## ROM[ROM[X]]=Y
A=Y
D=A
A=X
A=M
M=D

->

@Y
D=A
@X
A=M
M=D

# ROM[SP]=ROM[SP]+1
A=SP
M=M+1

# pop to D
ROM[SP]=ROM[SP]-1
// 
@0
M=M-1

# D=X
A=X
D=A

# D=ROM[ROM[SP]]
A=ROM[ROM[SP]]
D=A

# A=ROM[X]
A=X
A=M

# A=ROM[ROM[Y]]
A=ROM[Y]
A=M
->
A=Y
A=M
A=M

# D=ROM[ROM[SP]]
A=SP
A=M
A=M
D=A

# ROM[SP]=ROM[SP]-1
A=SP
M=M-1

## ROM[ROM[SP]]=ROM[ROM[SP]]+D
A=SP
A=M
M=M+D

# eq ってどうやればいいんだ？

引き算して0なら-1(true)をセットして、そうでないなら0(false)をセットすればいい。
・引き算する
・0ならLABEL-THENへ
・0 をセットする
・LABEL-ENDIF へ
LABEL-THEN
・-1 をセットする
LABEL-ENDIF
・〜