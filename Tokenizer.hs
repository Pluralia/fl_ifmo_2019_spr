module Tokenizer where

import Data.Char (toUpper, isDigit)


data Token = Ident String
           | KeyWord String
           | Number Int  -- Change Number type if you work with something other than Int
           deriving (Show, Eq)


--Keywords
keyWords :: [String]
keyWords = [ "False", "await", "else", "import", "pass", "None", "break", "except", "in", "raise", "True", "class", "finally", "is", "return", "and", "continue", "for", "lambda", "try", "as", "def", "from", "nonlocal", "while", "assert", "del", "global", "not", "with", "async", "elif", "if", "or", "yield" ]


--Identificators
letter :: Char -> Bool
letter = flip elem ['A'..'Z'] . toUpper

symbol :: Char -> Bool
symbol = (== '_')

digit :: Char -> Bool
digit = isDigit

start :: Char -> Bool
start x = letter x || symbol x

ident :: String -> Bool
ident []       = False
ident (x : xs) = and $ start x : (go <$> xs)
  where
    go :: Char -> Bool
    go y = letter y || symbol y || digit y


--Decimal integer numbers
numDigit :: Char -> Char
numDigit x
  | isDigit x = x
  | otherwise = error "Tokenizer failed: symbol is not a digit."

startZero :: Char -> Bool
startZero = (== '0')

getZero :: String -> Int
getZero []               = 0
getZero ('_' : '0' : xs) = getZero xs
getZero ('0' : xs)       = getZero xs
getZero _                = error "Tokenizer failed: token is not a zero."

getStrNumber :: String -> String
getStrNumber []             = []
getStrNumber ('_' : x : xs) = numDigit x : getStrNumber xs 
getStrNumber (x : xs)       = numDigit x : getStrNumber xs

decint :: String -> Int
decint str
  | null str        = error "Tokenizer failed: empty token"
  | (x : xs) <- str = if startZero x
                        then getZero xs
                        else read $ numDigit x : getStrNumber xs


--Main
tokenize :: String -> [Token]
tokenize = fmap toToken . words
  where
    toToken :: String -> Token
    toToken str
      | str `elem` keyWords = KeyWord str
      | ident str           = Ident str
      | otherwise           = Number $ decint str
