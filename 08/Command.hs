module Command where

data CommandType = PUSH_COMMAND | POP_COMMAND | GOTO_COMMAND | CALC_COMMAND | LABEL_COMMAND | IF_GOTO_COMMAND deriving (Show, Eq)
type Command = String

toArgs :: String -> [String]
toArgs str = split str

commandType :: Command -> CommandType
commandType command = case (split command) !! 0 of
    "push" -> PUSH_COMMAND
    "pop"  -> POP_COMMAND
    "goto" -> GOTO_COMMAND
    "label" -> LABEL_COMMAND
    "if-goto" -> IF_GOTO_COMMAND
    _      -> CALC_COMMAND

operand command = split command !! 0
segment command = split command !! 1
label command = split command !! 1
value command = split command !! 2

split = _split []
_split ts "" = ts
_split ts s = _split (ts ++ [(token s)]) (drop (length(token s) + 1) s)
 
token = _token ""
_token ys "" = ys
_token ys (x:xs) = do
  if x == ' '
    then ys
    else _token (ys ++ [x]) xs