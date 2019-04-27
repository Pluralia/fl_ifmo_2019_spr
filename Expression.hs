module Expression where

import Combinators
import Text.Printf
import Data.Char (isDigit, isSpace)
import Data.Either (fromLeft, fromRight)

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
              | Neg
              | Not

-- Simplest abstract syntax tree for expressions: only binops are allowed
data EAst a = BinOp Operator (EAst a) (EAst a)
            | Unary Operator (EAst a)
            | Variable String
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
  show Neg   = "-"
  show Not   = "!"

instance Show a => Show (EAst a) where
  show = show' 0
    where
      show' n t =
        (if n > 0 then printf "%s|_%s" (concat (replicate (n - 1) "| ")) else id)
        (case t of
                  BinOp op l r  -> printf "%s\n%s\n%s" (show op) (show' (ident n) l) (show' (ident n) r)
                  Unary op x    -> printf "%s\n%s" (show op) (show' (ident n) x)
                  Variable name -> name
                  Primary x     -> show x)
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
  runParserUntilEof (expression parseSpec parseAtom) input

-- Change the signature if necessary
-- Calculates the value of the input expression
executeExpression :: String -> Either String Integer -- (EAst Integer)
executeExpression input = 
  runParserUntilEof (expression execSpec execAtom) input

------------------------------------------------------------------------------------------------------

data Assoc = UAssoc -- unary
           | LAssoc -- left associativity
           | RAssoc -- right associativity
           | NAssoc -- not associative


-- General parser combinator for expressions
-- Binary operators are listed in the order of precedence (from lower to higher)
-- Binary operators on the same level of precedence have the same associativity
-- Binary operator is specified with a parser for the operator itself and a semantic function to apply to the operands
expression :: [(Assoc, [(Parser Char ErrorType b, Either (a -> a) (a -> a -> a))])] -> 
              Parser Char ErrorType a ->
              Parser Char ErrorType a
expression ops primary = go ops primary
  where
    str2UnOp :: (Parser Char ErrorType b, Either (a -> a) (a -> a -> a)) ->
                Parser Char ErrorType (a -> a)
    str2UnOp (parser, output) = parser >> (return . fromLeft undefined $ output)
    
    str2BinOp :: (Parser Char ErrorType b, Either (a -> a) (a -> a -> a)) ->
                Parser Char ErrorType (a -> a -> a)
    str2BinOp (parser, output) = parser >> (return . fromRight undefined $ output)
   
    parseUnOp :: [(Parser Char ErrorType b, Either (a -> a) (a -> a -> a))] ->
                Parser Char ErrorType (a -> a)
    parseUnOp = foldr1 (flip (<|>)) . fmap str2UnOp
    
    parseBinOp :: [(Parser Char ErrorType b, Either (a -> a) (a -> a -> a))] ->
                  Parser Char ErrorType (a -> a -> a)
    parseBinOp = foldr1 (flip (<|>)) . fmap str2BinOp

    parseNextOp assocRest = go assocRest primary
    
    go ((UAssoc, opsInfo@(_ : _)) : assocRest) pimary = do
      op <- parseUnOp opsInfo
      parseSpaces
      x  <- parseNextOp assocRest
      return $ op x

    go ((LAssoc, opsInfo@(_ : _)) : assocRest) primary = do
      a  <- parseNextOp assocRest
      bs <- many $ do
        parseSpaces
        op <- parseBinOp opsInfo
        parseSpaces
        x  <- parseNextOp assocRest
        return (x, op)
      return $ foldr (\(b, op) acc -> op acc b) a bs
   
    go ((RAssoc, opsInfo@(_ : _)) : assocRest) primary = do
      as <- many $ do
        x  <- parseNextOp assocRest
        parseSpaces
        op <- parseBinOp opsInfo
        parseSpaces
        return (x, op)
      b  <- parseNextOp assocRest
      return $ foldr (\(a, op) bcc -> op a bcc) b as

    go ((NAssoc, opsInfo@(_ : _)) : assocRest) primary = (do
        a  <- parseNextOp assocRest
        parseSpaces
        op <- parseBinOp opsInfo
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

parseDigit :: Parser Char ErrorType Char
parseDigit = like isDigit

parseNumber :: Parser Char ErrorType (EAst Integer)
parseNumber = Primary . read <$> some parseDigit

parseLetter :: Parser Char ErrorType Char
parseLetter = like (\x -> (x `elem` ['a'..'z']) || (x `elem` ['A'..'Z']))

parseSymbol :: Parser Char ErrorType Char
parseSymbol = like (== '_')

parseStartVariable :: Parser Char ErrorType Char
parseStartVariable =
  parseLetter <|> parseSymbol

parseBodyVariable :: Parser Char ErrorType String
parseBodyVariable = 
      (some parseLetter)
  <|> (some parseSymbol)
  <|> (some parseDigit)

parseVariable :: Parser Char ErrorType (EAst Integer)
parseVariable = do
  x  <- parseStartVariable
  xs <- concat <$> many parseBodyVariable
  return $ Variable (x : xs)

------------------------------------------------------------------------------------------------------

parseAtom :: Parser Char ErrorType (EAst Integer)
parseAtom = parseNumber <|> parseVariable

parseSpec = [ (RAssoc, [ (tokList "||", Right (BinOp Disj))
                       ])
            , (RAssoc, [ (tokList "&&", Right (BinOp Conj))
                       ])
            , (NAssoc, [ (tokList "==", Right (BinOp Eq))
                       , (tokList "/=", Right (BinOp Neq))
                       , (tokList "<" , Right (BinOp Lt))
                       , (tokList "<=", Right (BinOp Le))
                       , (tokList ">" , Right (BinOp Gt))
                       , (tokList ">=", Right (BinOp Ge))
                       ])
            , (LAssoc, [ (tokList "+", Right (BinOp Sum))
                       , (tokList "-", Right (BinOp Minus))
                       ])
            , (LAssoc, [ (tokList "*", Right (BinOp Mul))
                       , (tokList "/", Right (BinOp Div))
                       ])
            , (RAssoc, [ (tokList "^", Right (BinOp Pow)) 
                       ])
            , (UAssoc, [ (tokList "-", Left (Unary Neg))
                       , (tokList "!", Left (Unary Not))
                       ])
            ]

------------------------------------------------------------------------------------------------------

i2b :: Integer -> Bool
i2b 0 = True
i2b _ = False

b2i :: Bool -> Integer
b2i True  = 1
b2i False = 0

execAtom :: Parser Char ErrorType Integer
execAtom = read <$> some (like isDigit)

execSpec = [ (RAssoc, [ (tokList "||", Right (\a b -> b2i $ i2b a || i2b b))
                      ])
           , (RAssoc, [ (tokList "&&", Right (\a b -> b2i $ i2b a && i2b b))
                      ])
           , (NAssoc, [ (tokList "==", Right (\a b -> b2i $ a == b))
                      , (tokList "/=", Right (\a b -> b2i $ a /= b))
                      , (tokList "<" , Right (\a b -> b2i $ a <  b))
                      , (tokList "<=", Right (\a b -> b2i $ a <= b))
                      , (tokList ">" , Right (\a b -> b2i $ a >  b))
                      , (tokList ">=", Right (\a b -> b2i $ a >= b))
                      ])
           , (LAssoc, [ (tokList "+", Right (+))
                      , (tokList "-", Right (-))
                      ])
           , (LAssoc, [ (tokList "*", Right (*))
                      , (tokList "/", Right div)
                      ])
           , (RAssoc, [ (tokList "^", Right (^))  
                      ])
           , (UAssoc, [ (tokList "-", Left (0 -))
                      , (tokList "!", Left (b2i . not . i2b))
                      ])
           ]

