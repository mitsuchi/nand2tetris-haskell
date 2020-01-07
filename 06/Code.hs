module Code where

import Data.Char

dest :: String -> String
dest mnem = dest' mnem "000"

dest' :: String -> String -> String
dest' "" bits = bits
dest' mnem bits =
    if elem 'A' mnem
        then dest' (remove 'A' mnem) (update bits 0 "1")
        else if elem 'D' mnem
            then dest' (remove 'D' mnem) (update bits 1 "1")
            else if elem 'M' mnem
                then dest' (remove 'M' mnem) (update bits 2 "1")
                else bits

remove :: Char -> String -> String
remove char xs = filter (not . (\x -> x == char)) xs

update :: String -> Int -> String -> String
update str i v = take i str ++ v ++ drop (i+1) str

comp :: String -> String
comp str = case str of
    "0"   -> "0101010"
    "1"   -> "0111111"
    "-1"  -> "0111010"
    "D"   -> "0001100"
    "A"   -> "0110000"
    "M"   -> "1110000"
    "!D"  -> "0001101"
    "!A"  -> "0110001"
    "!M"  -> "1110001"
    "-D"  -> "0001111"
    "-A"  -> "0110011"
    "-M"  -> "1110011"
    "D+1" -> "0011111"
    "A+1" -> "0110111"
    "M+1" -> "1110111"
    "D-1" -> "0001110"
    "A-1" -> "0110010"
    "M-1" -> "1110010"
    "D+A" -> "0000010"
    "A+D" -> "0000010"
    "D+M" -> "1000010"
    "M+D" -> "1000010"
    "D-A" -> "0010011"
    "D-M" -> "1010011"
    "A-D" -> "0000111"
    "M-D" -> "1000111"
    "D&A" -> "0000000"
    "A&D" -> "0000000"
    "D&M" -> "1000000"
    "M&D" -> "1000000"
    "D|A" -> "0010101"
    "A|D" -> "0010101"
    "D|M" -> "1010101"
    "M|D" -> "1010101"

jump :: String -> String
jump str = case str of
    ""    -> "000"
    "JGT" -> "001"
    "JEQ" -> "010"
    "JGE" -> "011"
    "JLT" -> "100"
    "JNE" -> "101"
    "JLE" -> "110"
    "JMP" -> "111"
