module Combinators where

-- Parsing result is either an error message or some payload and a suffix of the input which is yet to be parsed
newtype Parser str ok err = Parser { runParser :: str -> Either err (str, ok) }
