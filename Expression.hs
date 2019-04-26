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
------------------------------------------------------------------------------------------------------

-- Change the signature if necessary
-- Constructs AST for the input expression
parseExpression :: String -> Either String (EAst Integer)
parseExpression input = 
  runParserUntilEof (expression spec parsePrimary) input

-- Change the signature if necessary
-- Calculates the value of the input expression
executeExpression :: String -> Either String Integer
executeExpression input = 
  runParserUntilEof (expression execSpec execPrimary) input

------------------------------------------------------------------------------------------------------

data Assoc = LAssoc -- left associativity
           | RAssoc -- right associativity
           | NAssoc -- not associative


-- General parser combinator for expressions
-- Binary operators are listed in the order of precedence (from lower to higher)
-- Binary operators on the same level of precedence have the same associativity
-- Binary operator is specified with a parser for the operator itself and a semantic function to apply to the operands
expression :: [(Assoc, [(Parser Char ErrorType b, a -> a -> a)])] -> 
              Parser Char ErrorType a ->
              Parser Char ErrorType a
expression ops primary = go ops primary
  where
    str2op (parser, output) = parser >> return output
    parseOp = foldr1 (flip (<|>)) . fmap str2op
    parseNextOp assocRest = go assocRest primary

    go ((LAssoc, opsInfo@(_ : _)) : assocRest) primary = do
      a  <- parseNextOp assocRest
      bs <- many $ do
        parseSpaces
        op <- parseOp opsInfo
        parseSpaces
        x  <- parseNextOp assocRest
        return (x, op)
      return $ foldr (\(b, op) acc -> op acc b) a bs
   
    go ((RAssoc, opsInfo@(_ : _)) : assocRest) primary = do
      as <- many $ do
        x  <- parseNextOp assocRest
        parseSpaces
        op <- parseOp opsInfo
        parseSpaces
        return (x, op)
      b  <- parseNextOp assocRest
      return $ foldr (\(a, op) bcc -> op a bcc) b as

    go ((NAssoc, opsInfo@(_ : _)) : assocRest) primary = (do
        a  <- parseNextOp assocRest
        parseSpaces
        op <- parseOp opsInfo
        parseSpaces
        b  <- parseNextOp assocRest
        return $ op a b)
      <|> parseNextOp assocRest
 
    go _ primary = primary
      <|> (parseLbr
         *> parseNextOp ops
        <*  parseRbr)


runParserUntilEof :: Parser Char ErrorType ok -> String -> Either String ok 
runParserUntilEof parser input =
    either
      (Left . err2str)
      (\(rest, ok) -> if null rest then Right ok else Left "Expected eof") 
      (runParser parser . str2batch $ input)

str2batch :: String -> [Batch Char]
str2batch input = concat $
  (\(l, symbs) -> (\(n, symb) -> Batch symb (l, n)) <$> zip [1..] symbs) <$> zip [1..] (lines input)

err2str :: [(ErrorType, Holder)] -> String
err2str = concatMap errType2str
  where
    errType2str :: (ErrorType, Holder) -> String
    errType2str (err, (l, n)) = show err ++ ":" ++ show l ++ ":" ++ show n ++ "\n"

------------------------------------------------------------------------------------------------------
-- helpers
parseSpaces :: Parser Char ErrorType String
parseSpaces = many (like isSpace)

parseLbr :: Parser Char ErrorType Char
parseLbr = like (== '(')

parseRbr :: Parser Char ErrorType Char
parseRbr = like (== ')')

------------------------------------------------------------------------------------------------------

parsePrimary :: Parser Char ErrorType (EAst Integer)
parsePrimary = Primary . read <$>
  some (like isDigit)

spec = [ (RAssoc, [ (tokList "||", BinOp Disj)
                  ])
       , (RAssoc, [ (tokList "&&", BinOp Conj)
                  ])
       , (NAssoc, [ (tokList "==", BinOp Eq)
                  , (tokList "/=", BinOp Neq)
                  , (tokList "<", BinOp Lt)
                  , (tokList "<=", BinOp Le)
                  , (tokList ">", BinOp Gt)
                  , (tokList ">=", BinOp Ge)
                  ])
       , (LAssoc, [ (tokList "+", BinOp Sum)
                  , (tokList "-", BinOp Minus)
                  ])
       , (LAssoc, [ (tokList "*", BinOp Mul)
                  , (tokList "/", BinOp Div)
                  ])
       , (RAssoc, [ (tokList "^", BinOp Pow)  
                  ])
       ]

------------------------------------------------------------------------------------------------------

i2b :: Integer -> Bool
i2b 0 = True
i2b _ = False

b2i :: Bool -> Integer
b2i True  = 1
b2i False = 0

execPrimary :: Parser Char ErrorType Integer
execPrimary = read <$>
  some (like isDigit)

execSpec = [ (RAssoc, [ (tokList "||", (\a b -> b2i $ i2b a || i2b b))
                      ])
           , (RAssoc, [ (tokList "&&", (\a b -> b2i $ i2b a && i2b b))
                      ])
           , (NAssoc, [ (tokList "==", (\a b -> b2i $ a == b))
                      , (tokList "/=", (\a b -> b2i $ a /= b))
                      , (tokList "<",  (\a b -> b2i $ a <  b))
                      , (tokList "<=", (\a b -> b2i $ a <= b))
                      , (tokList ">",  (\a b -> b2i $ a >  b))
                      , (tokList ">=", (\a b -> b2i $ a >= b))
                      ])
           , (LAssoc, [ (tokList "+", (+))
                      , (tokList "-", (-))
                      ])
           , (LAssoc, [ (tokList "*", (*))
                      , (tokList "/", div)
                      ])
           , (RAssoc, [ (tokList "^", (^))  
                      ])
           ]

