module Command where

data CommandType = A_COMMAND | C_COMMAND | L_COMMAND deriving (Show, Eq)
type Command = String
