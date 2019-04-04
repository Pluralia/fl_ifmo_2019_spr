module Expression where

import Combinators
import Text.Printf
import Safe (readMay)

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

ignore2 :: a -> b -> c -> a
ignore2 res _ _ = res

parsePow :: Parser Char ErrorType Operator
parsePow = const Pow <$> like (== '^')

parseMul :: Parser Char ErrorType Operator
parseMul = const Mul <$> like (== '*')

parseDiv :: Parser Char ErrorType Operator
parseDiv = const Div <$> like (== '/')

parseSum :: Parser Char ErrorType Operator
parseSum = const Sum <$> like (== '+')

parseMinus :: Parser Char ErrorType Operator
parseMinus = const Minus <$> like (== '-')

parseEq :: Parser Char ErrorType Operator
parseEq = ignore2 Eq
  <$> like (== '=')
  <*> like (== '=')

parseNeq :: Parser Char ErrorType Operator
parseNeq = ignore2 Neq
  <$> like (== '\\')
  <*> like (== '=')

parseLe :: Parser Char ErrorType Operator
parseLe = ignore2 Le
  <$> like (== '<')
  <*> like (== '=')

parseLt :: Parser Char ErrorType Operator
parseLt = const Lt <$> like (== '<')

parseGe :: Parser Char ErrorType Operator
parseGe = ignore2 Ge
  <$> like (== '>')
  <*> like (== '=')

parseGt :: Parser Char ErrorType Operator
parseGt = const Gt <$> like (== '>')

parseConj :: Parser Char ErrorType Operator
parseConj = ignore2 Conj
  <$> like (== '&')
  <*> like (== '&')

parseDisj :: Parser Char ErrorType Operator
parseDisj = ignore2 Disj
  <$> like (== '|')
  <*> like (== '|')


-- Simplest abstract syntax tree for expressions: only binops are allowed
parsePrimary :: Parser Char ErrorType (EAst Integer)
parsePrimary = Primary . read <$>
  some (like (`elem` ['0'..'9']))

parseOrRule :: Parser Char ErrorType (EAst Integer)
parseOrRule = (flip BinOp
    <$> parseAndRule
    <*> parseDisj
    <*> parseOrRule)
  <|> parsePrimary

parseAndRule :: Parser Char ErrorType (EAst Integer)
parseAndRule = (flip BinOp
    <$> parseOrdRule
    <*> parseConj
    <*> parseAndRule)
  <|> parsePrimary

parseOrdRule :: Parser Char ErrorType (EAst Integer)
parseOrdRule = (flip BinOp
    <$> parseOrdRule
    <*> (parseEq <|> parseNeq <|> parseLe <|> parseLt <|> parseGe <|> parseGt)
    <*> parseOrdRule)
  <|> parsePrimary

parseSumRule :: Parser Char ErrorType (EAst Integer)
parseSumRule = (flip BinOp
    <$> parseSumRule
    <*> (parseSum <|> parseMinus)
    <*> parseMulRule)
  <|> parsePrimary

parseMulRule :: Parser Char ErrorType (EAst Integer)
parseMulRule = (flip BinOp
    <$> parseMulRule
    <*> (parseMul <|> parseDiv)
    <*> parsePowRule)
  <|> parsePrimary

parsePowRule :: Parser Char ErrorType (EAst Integer)
parsePowRule = (flip BinOp
    <$> parsePrimary
    <*> parsePow
    <*> parsePowRule)
  <|> parsePrimary


parseExpression' :: Parser Char ErrorType (EAst Integer)
parseExpression' = parseOrRule


str2Batch :: String -> [Batch Char]
str2Batch input = concat $
  (\(l, symbs) -> (\(n, symb) -> Batch symb (l, n)) <$> zip [1..] symbs) <$> zip [1..] (lines input)


err2str :: [(ErrorType, Holder)] -> String
err2str = concatMap errType2str
  where
    errType2str :: (ErrorType, Holder) -> String
    errType2str (err, (l, n)) = show err ++ ":" ++ show l ++ ":" ++ show n ++ "\n"

-- Change the signature if necessary
parseExpression :: String -> Either String (EAst Integer)
parseExpression =
  either (Left . err2str) (Right . snd) . runParser parseExpression' . str2Batch

