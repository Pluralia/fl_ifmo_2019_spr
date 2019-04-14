module ExprCombinators where

-- Parsing result is either an error message or some payload and a suffix of the input which is yet to be parsed
newtype Parser str ok err = Parser { runParser :: str -> Either err (str, ok) }

data Assoc = LAssoc -- left associativity
           | RAssoc -- right associativity
           | NAssoc -- not associative

-- General parser combinator for expressions
-- Binary operators are listed in the order of precedence (from lower to higher)
-- Binary operators on the same level of precedence have the same associativity
-- Binary operator is specified with a parser for the operator itself and a semantic function to apply to the operands
expression :: [(Assoc, [(Parser str b err, a -> a -> a)])] -> 
              Parser str a err ->
              Parser str a err
expression ops primary = undefined 

runParserUntilEof :: Foldable t => Parser (t str) ok String -> (t str) -> Either String ok 
runParserUntilEof p inp = 
  either (Left . id) (\(rest, ok) -> if null rest then Right ok else Left "Expected eof") (runParser p inp)
