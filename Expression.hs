module Expression where

import Text.Printf
import Combinators

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

-- Change the signature if necessary
-- Constructs AST for the input expression
parseExpression :: String -> Either String (EAst Integer)
parseExpression input = 
  runParserUntilEof (expression spec parsePrimary) input

-- Change the signature if necessary
-- Calculates the value of the input expression
executeExpression :: String -> Either String Integer
executeExpression input = 
  runParserUntilEof (expression undefined undefined) . str2batch $ input

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

str2Batch :: String -> [Batch Char]
str2Batch input = concat $
  (\(l, symbs) -> (\(n, symb) -> Batch symb (l, n)) <$> zip [1..] symbs) <$> zip [1..] (lines input)

err2str :: [(ErrorType, Holder)] -> String
err2str = concatMap errType2str
  where
    errType2str :: (ErrorType, Holder) -> String
    errType2str (err, (l, n)) = show err ++ ":" ++ show l ++ ":" ++ show n ++ "\n"

-----------------------------------------------------------------------------------------------------

parsePrimary :: Parser Char ErrorType (EAst Integer)
parsePrimary = Primary . read <$>
  some (like isDigit)

spec = [ (RAssoc, [ (parseDisj, BinOp Disj)
                  ]
         )
       , (RAssoc, [ (parseConj, BinOp Conj)
                  ]
         )
       , (NAssoc, [ (parseEq, BinOp Eq)
                  , (parseNeq, BinOp Neq)
                  , (parseLe, BinOp Le)
                  , (parseLt, BinOp Lt)
                  , (parseGe, BinOp Ge)
                  , (parseGt, BinOp Gt)
                  ]
         )
       , (LAssoc, [ (parseSum, BinOp Sum)
                  , (parseMinus, BinOp Minus)
                  ]
         )
       , (LAssoc, [ (parseMul, BinOp Mul)
                  , (parseDiv, BinOp Div)
                  ]
         )
       , (RAssoc, [ (parsePow, BinOp Pow)
                  ]
         )
       ]


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

