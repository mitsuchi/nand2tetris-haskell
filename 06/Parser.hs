module Parser where

import Data.Char
import System.IO
import Control.Monad
import Command

commandType :: Command -> CommandType
commandType ('@':_) = A_COMMAND
commandType ('(':_) = L_COMMAND
commandType _       = C_COMMAND

removeSpace xs = filter (not . isSpace) xs

comment :: String -> Bool
comment ('/':_ ) = True
comment _        = False

symbol :: String -> String
symbol ('@':sym) = sym
symbol ('(':sym) = init sym

dest :: Command -> String
dest command = if elem '=' command
    then takeWhile (/= '=') command
    else ""

comp :: Command -> String
comp command = if elem '=' command
    then takeWhile (\x -> x /= ';') . dropWhile (== '=') . dropWhile (/= '=') $ command
    else takeWhile (/= ';') command

jump :: Command -> String
jump = dropWhile (== ';') . dropWhile (/= ';')


