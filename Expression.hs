module Expression where

import Combinators
import Text.Printf
import Data.Char (isDigit, isSpace)

data Operator = Pow
              | Mul
              | Div
              | Sum
              | Minus
              | Eq
              | Neq
              | Le
              | Lt
              | Ge
              | Gt
              | Conj
              | Disj

-- Simplest abstract syntax tree for expressions: only binops are allowed
data EAst a = BinOp Operator (EAst a) (EAst a)
            | Primary a

data ExprError = ErrorRest
  deriving (Show)

instance Show Operator where
  show Pow   = "^"
  show Mul   = "*"
  show Div   = "/"
  show Sum   = "+"
  show Minus = "-"
  show Eq    = "=="
  show Neq   = "/="
  show Le    = "<="
  show Lt    = "<"
  show Ge    = ">="
  show Gt    = ">"
  show Conj  = "&&"
  show Disj  = "||"

instance Show a => Show (EAst a) where
  show = show' 0
    where
      show' n t =
        (if n > 0 then printf "%s|_%s" (concat (replicate (n - 1) "| ")) else id)
        (case t of
                  BinOp op l r -> printf "%s\n%s\n%s" (show op) (show' (ident n) l) (show' (ident n) r)
                  Primary x -> show x)
      ident = (+1)

{-
show (BinOp Conj (BinOp Pow (Primary 1) (BinOp Sum (Primary 2) (Primary 3))) (Primary 4))

&&
|_^
| |_1
| |_+
| | |_2
| | |_3
|_4
-}


-- helpers
parsePrimary :: Parser Char ErrorType (EAst Integer)
parsePrimary = Primary . read <$>
  some (like isDigit)

parseSpaces :: Parser Char ErrorType String
parseSpaces = some (like isSpace)

parseLbr :: Parser Char ErrorType Char
parseLbr = like (== '(')

parseRbr :: Parser Char ErrorType Char
parseRbr = like (== ')')


-- Simplest abstract syntax tree for expressions: only binops are allowed
parseOrRule :: Parser Char ErrorType (EAst Integer)
parseOrRule = do
  as <- many $ do
    x  <- parseAndRule
    parseSpaces
    tokList "||"
    parseSpaces
    return x
  b  <- parseAndRule
  return $ foldr (\a acc -> BinOp Disj a acc) b as


parseAndRule :: Parser Char ErrorType (EAst Integer)
parseAndRule = do
  as <- many $ do
    x  <- parseOrdRule
    parseSpaces
    tokList "&&"
    parseSpaces
    return x
  b  <- parseOrdRule
  return $ foldr (\a acc -> BinOp Conj a acc) b as


parseOrdRule :: Parser Char ErrorType (EAst Integer)
parseOrdRule = (do
    let str2Op op = case op of
                      "==" -> Eq
                      "/=" -> Neq
                      "<=" -> Le
                      "<"  -> Lt
                      ">=" -> Ge
                      ">"  -> Gt
    a <- parseSumRule
    parseSpaces
    op <- tokList "==" <|> tokList "/="
      <|> tokList "<=" <|> tokList "<"
      <|> tokList ">=" <|> tokList ">"
    parseSpaces
    b  <- parseSumRule
    return $ BinOp (str2Op op) a b)
  <|> parseSumRule


parseSumRule :: Parser Char ErrorType (EAst Integer)
parseSumRule = do
  let str2Op op = case op of
                    "+" -> Sum
                    "-" -> Minus
  as <- many $ do
    x  <- parseProdRule
    parseSpaces
    op <- tokList "+" <|> tokList "-"
    parseSpaces
    return (x, op)
  b  <- parseProdRule
  return $ foldr (\(a, op) acc -> BinOp (str2Op op) a acc) b as


parseProdRule :: Parser Char ErrorType (EAst Integer)
parseProdRule = do
  let str2Op op = case op of
                    "*" -> Mul
                    "/" -> Div
  as <- many $ do
    x  <- parseDegRule
    parseSpaces
    op <- tokList "*" <|> tokList "/"
    parseSpaces
    return (x, op)
  b  <- parseDegRule
  return $ foldr (\(a, op) acc -> BinOp (str2Op op) a acc) b as


parseDegRule :: Parser Char ErrorType (EAst Integer)
parseDegRule = do
  as <- many $ do
    x  <- parseFinRule
    parseSpaces
    tokList "^"
    parseSpaces
    return x
  b  <- parseFinRule
  return $ foldr (\a acc -> BinOp Pow a acc) b as


parseFinRule :: Parser Char ErrorType (EAst Integer)
parseFinRule = parsePrimary
  <|> (parseLbr
     *> parseOrRule
    <*  parseRbr)

------------------------------------------------------------------------------------------------------

parseExpression' :: Parser Char ErrorType (EAst Integer)
parseExpression' = parseOrRule
  <*  notParser (like (const True))

str2batch :: String -> [Batch Char]
str2batch input = concat $
  (\(l, symbs) -> (\(n, symb) -> Batch symb (l, n)) <$> zip [1..] symbs) <$> zip [1..] (lines input)

err2str :: [(ErrorType, Holder)] -> String
err2str = concatMap errType2str
  where
    errType2str :: (ErrorType, Holder) -> String
    errType2str (err, (l, n)) = show err ++ ":" ++ show l ++ ":" ++ show n ++ "\n"

-- Change the signature if necessary
parseExpression :: String -> Either String (EAst Integer)
parseExpression =
  either (Left . err2str) (Right . snd) . runParser parseExpression' . str2batch
