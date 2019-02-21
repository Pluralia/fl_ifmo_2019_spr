module Tokenizer where

import Combinators
import Data.Char (isDigit, ord)
import Data.List (foldl')
import Data.Maybe (fromJust)


data Token = Ident String
           | KeyWord String
           | Number Integer  -- Change Number type if you work with something other than Int
           deriving (Show, Eq)


--Keywords
keyWords :: [String]
keyWords = [ "False", "await", "else", "import", "pass", "None", "break", "except", "in", "raise", "True", "class", "finally", "is", "return", "and", "continue", "for", "lambda", "try", "as", "def", "from", "nonlocal", "while", "assert", "del", "global", "not", "with", "async", "elif", "if", "or", "yield" ]


--Parses Tokens
tokenize :: String -> [Token]
tokenize input =
  case runParser parseTokens input of
    Just ([], res) -> res
    _              -> []

parseOneToken :: Parser String Token
parseOneToken = (Ident <$> parseIdent)
  <|> (KeyWord <$> parseKeyWord)
  <|> (Number <$> parseNumber)

parseDelimiters :: Parser String String
parseDelimiters = some (token ' ')

parseTokens :: Parser String [Token]
parseTokens = many parseDelimiters *>
      ((:) <$> parseOneToken)
  <*> many (some parseDelimiters *> parseOneToken)
  <*  many parseDelimiters


--Parse Ident
parseLetter :: Parser String Char
parseLetter = like (\x -> (x `elem` ['a'..'z']) || (x `elem` ['A'..'Z']))

parseSymbol :: Parser String Char
parseSymbol = token '_'

parseDigit :: Parser String Char
parseDigit = like isDigit

parseStartIdent :: Parser String Char
parseStartIdent =
  parseLetter <|> parseSymbol

parseBodyIdent :: Parser String String
parseBodyIdent =
      (some parseLetter)
  <|> (some parseSymbol)
  <|> (some parseDigit)

parseIdent :: Parser String String
parseIdent = (:)
  <$> parseStartIdent
  <*> (concat <$> many parseBodyIdent)

-- Parses Keywords
parseKeyWord :: Parser String String
parseKeyWord = keywords keyWords


-- Parses Numbers
parseDigitBlock :: Parser String Char
parseDigitBlock =
      (token '_' *> like isDigit)
  <|> like isDigit

parseZeroBlock :: Parser String Char
parseZeroBlock =
      (token '_' *> token '0')
  <|> token '0'

myRead :: String -> Integer
myRead input
  | null $ filter (not . isDigit) input = foldl' (\acc x -> 10 * acc + toInteger (ord x) - 48) 0 input
  | otherwise                           = undefined

parseNumber :: Parser String Integer
parseNumber = read
  <$> (
        ((:)
           <$> like (\x -> isDigit x && (x /= '0'))
           <*> many parseDigitBlock
        )
      <|>
        (const "0"
           <$> some (like (== '0'))
           <*  many parseZeroBlock
        )
      )

