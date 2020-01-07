# CPUをつくる

制御フラグでわかってないところをCPUの仕様を読みながら埋めていこう。

# A register
まず A Register の load は、単に inst[15] = 0 のときだけではなく
instruction の d1 つまり inst[5] = 1 のときも 1 になる。
つまり inst[5] = 1 or inst[15] = 0 だ

# D Register
D の load は instruction の d2 つまり inst[4] = 1 ? でいい

# Mux left
A register の左側にある Mux の sel は計算結果と即値のどっちを使うかで、
つまりはA命令なら即値、C命令なら計算結果を使うので、A命令かどうか？（inst[15] = 0 ?） でいい。

a は aluOut のままでいい。
b は A命令の inst[0..14] の先頭に 0 をつけたもの。つまり inst のままでいい。
よくできてるな。

# writeM
メモリに書くかどうか？は inst の d3 つまり inst[3] = 1 ? でいい。
と思ったらだめだった。C命令のときに限るんだから、
inst[3] = 1 and inst[15] = 1 だ

# PC
ジャンプ条件を満たすかどうかは、
j1 j2 j3 の条件を or で結べばいい。だから 1 1 1 のときは無条件ジャンプになる。
よくできてる。j1 = inst[2], j2 = inst[1], j3 = inst[0]

(j1 and ng) or (j2 and zr) or (j3 and not ng)
こうだ。
だめだった。ジャンプするのはC命令のときだけだ。
inst[15] = 1 and ((j1 and ng) or (j2 and zr) or (j3 and not ng))
だ。
間違ってた！
正かどうかは not ng じゃない。not ng and not zr だ。

# instruction と bit の対応表
15 14 13 12 11 10  9  8  7  6  5  4  3  2  1  0
1  1  1  a  c1 c2 c3 c4 c5 c6 d1 d2 d3 j1 j2 j3

# やったーできたぞー

File.open do |f|
    f.puts "hoge"
end

File.open { file -> {
    file.puts "hoge"
}}

File.open { f ->
    f.puts "hoge"
    f.puts "world"
}

やっぱりオフサイドルールでかけたほうがいいか。
