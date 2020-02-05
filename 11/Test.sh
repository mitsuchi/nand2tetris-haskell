runhaskell Main.hs $1 > tmp-test.vm
bash ../../tools/JackCompiler.sh $1
diff -u ${1/.jack/.vm} tmp-test.vm
rm ${1/.jack/.vm} tmp-test.vm