module Parser where

import Data.Char
import System.IO
import Control.Monad

trimSpace xs = reverse . dropWhile isSpace . reverse . takeWhile ( /= '/') . dropWhile isSpace $ xs

comment :: String -> Bool
comment ('/':_ ) = True
comment _        = False

symbol :: String -> String
symbol ('@':sym) = sym
symbol ('(':sym) = init sym

plines :: String -> [String]
plines []  = []
plines str = let (a, str') = breakNewline str
             in a : plines str'

breakNewline :: String -> (String, String)
breakNewline []       = ([], [])
breakNewline (x : xs) =
    case x of
        '\n' -> ([], xs)
        '\r' -> ([], case xs of
                         ('\n' : xs') -> xs'
                         _            -> xs)
        _    -> let (line, rest) = breakNewline xs
                in (x : line, rest)
