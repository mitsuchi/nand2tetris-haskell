module Main where

import Data.Char
import Data.List
import System.Environment
import System.IO
import Control.Monad
import qualified Parser as P
import qualified Code as C
import Command
import Text.Printf

main :: IO ()
main = do
    args <- getArgs
    let file = args !! 0
    content <- readFile file
    let commands = filter (\line -> (not . null) line && (not . P.comment) line) $ map P.removeSpace $ lines content
    let hack = getHack commands
    writeFile (changePostfix file "hack2") hack
    return ()

toBinary :: String -> String
toBinary decimal = printf "%015b" (read decimal :: Int)

getHack :: [String] -> String
getHack commands = intercalate "\n" (map (\command ->
        case P.commandType command of
            A_COMMAND -> "0" ++ toBinary (P.symbol command)
            C_COMMAND -> "111" ++ C.comp (P.comp command) ++ C.dest (P.dest command) ++ C.jump (P.jump command)
        ) commands) ++ "\n"

changePostfix :: String -> String -> String
changePostfix str postfix = (++ postfix ) . reverse . dropWhile (/= '.') . reverse $ str