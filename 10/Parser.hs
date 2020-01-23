module Parser where

import Data.Char  
import Data.List  
import StateT
import MonadTrans

type Parser a = StateT String (Either String) a

--anyChar :: StateT String (Either String) Char
anyChar :: Parser Char
anyChar = StateT $ anyChar where
    anyChar (x:xs) = Right (x, xs)
    anyChar _      = Left "too short"
 
many :: Parser a -> Parser [a]
--many :: StateT (String -> Either String a) -> StateT (String -> Either String [a])
-- (:) :: a -> ([a] -> [a])
-- (:) <$> :: Parser a -> Parser ([a] -> [a])
-- (:) <$> p :: Parser ([a] -> [a])
-- (:) <$> p <*> many p :: Parser [a]
many p = ((:) <$> p <*> many p) <|> (pure [])
-- take one char and glue it and take many char and glue it

some p = (:) <$> p <*> (some p <|> pure [])

--string (x:xs) = (:) <$> char x <*> string xs
string (x:xs) = do
    x1 <- char x
    xs1 <- string xs
    pure $ x1:xs1
string []     = pure []

satisfy :: (Char -> Bool) -> Parser Char
satisfy f = StateT $ satisfy where
    satisfy (x:xs) | not $ f x = Left $ ": " ++ show x
    satisfy xs                 = runStateT anyChar xs

-- endWith :: String -> String -> (Maybe String, String)
-- endWith end "" = (Nothing, "")
-- endWith end str =
--     if isPrefixOf end str
--     then (Just end, drop (length end) str)
--     else endWith end (tail str)

endWith :: String -> Parser String
endWith end = StateT $ endWith where
    endWith "" = Left $ ("endWith: " ++ end)
    endWith str =
        if isPrefixOf end str
        then Right (end, drop (length end) str)
        else endWith (tail str)

(StateT smas) <|> (StateT smbs) = StateT $ \s -> case (smas s, smbs s) of
    (Left a, Left b) -> Left $ b ++ a
    (Left a, Right b) -> Right b
    (Right a, _) -> Right a

--left x = StateT $ \s -> Left x
left :: String -> Parser a
left = lift . Left

char c = satisfy (== c) <|> left ("not char " ++ (show c))
digit = satisfy isDigit <|> left "not digit"
letter = satisfy isLetter <|> left "not letter"

--
oneOf :: [Char] -> Parser Char
oneOf s = satisfy (flip elem s)

noneOf :: [Char] -> Parser Char
noneOf s = satisfy (not . (flip elem s))

chainl :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainl p op a = (p `chainl1` op) <|> return a

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
p `chainl1` op = do {a <- p; rest a}
  where rest a = (do f <- op
                     b <- p
                     rest (f a b))
                 <|> return a

--
spaces :: Parser String
spaces = many $ oneOf " \n\r"

token :: Parser a -> Parser a
--token p = do { a <- p; spaces ; return a}
token p = p <* spaces

reserved :: String -> Parser String
reserved str = token $ string str

number :: Parser Int
number = do
  s <- string "-" <|> return []
  cs <- some digit
  return $ read (s ++ cs)
--number = read <$> ((++) <$> (string "-" <|> pure []) <*> some digit)

parens :: Parser a -> Parser a
parens m = do
  reserved "("
  n <- m
  reserved ")"
  pure n

test1 = do
    x1 <- anyChar
    x2 <- anyChar
    pure [x1, x2]

parse f s = case runStateT f s of
    Right (r, "") -> Right r
    Right (r, _)  -> Left "parser error: does not consume all text"
    Left e        -> Left $ "parser error: [" ++ show s ++ "] " ++ e