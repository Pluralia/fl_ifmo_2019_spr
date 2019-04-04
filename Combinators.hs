{-# LANGUAGE InstanceSigs  #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase    #-}

module Combinators
    ( Parser(..)  
    , Batch(..)
    , ErrorType(..)
    , Holder(..)
    , many
    , some
    , like
    , (<|>)
    , token
    , keywords 
    , success
    , notParser
    ) where

import Prelude hiding (seq)
import Control.Applicative
import Data.List (sort, partition)


-- number of the row and the column
type Holder = (Integer, Integer)

-- Errors in the stream
data ErrorType = EmptyInput
               | EmptyTrieKeywords
               | ErrorToken
               | FailTokenList
               | FailPredicate
               | StrangeRest
  deriving (Show, Eq)

data Batch str = Batch { input  :: str
                       , holder :: Holder }
  deriving (Show)

-- Parsing result is either an error message or some payload and a suffix of the input which is yet to be parsed
newtype Parser str err ok =
  Parser { runParser :: [Batch str] -> Either [(err, Holder)] ([Batch str], ok) }


-- Parser which always succeedes consuming no input
success :: ok -> Parser str err ok
success ok = Parser $ \s -> Right (s, ok)

-- Parser which fails no mater the input
failP :: err -> Parser str err ok
failP err = Parser $
  \case
    []          -> Left [(err, (0, 0))]
    (batch : _) -> Left [(err, holder batch)]


instance Functor (Parser str err) where
-- Applies a function to the parsing result, if parser succeedes
  fmap :: (a -> b) -> Parser str err a -> Parser str err b
  fmap f p = Parser $ \s ->
    case runParser p s of
      Left err      -> Left err
      Right (s', a) -> Right (s', f a)


instance Applicative (Parser str err) where
  pure :: ok -> Parser str err ok
  pure = success
-- Applicative sequence combinator
  (<*>) :: Parser str err (a -> b) -> Parser str err a -> Parser str err b
  p <*> q = Parser $ \s ->
    case runParser p s of
      Left err      -> Left err
      Right (s', f) -> runParser (f <$> q) s'


instance Alternative (Parser str err) where
  empty :: Parser str err ok
  empty = Parser $ const (Left [])
-- Biased choice: if the first parser succeedes, the second is never run
  (<|>) :: Parser str err ok -> Parser str err ok -> Parser str err ok
  p <|> q = Parser $ \s ->
    case runParser p s of
      Left errP -> case runParser q s of
                     Left errQ -> Left $ errP <> errQ
                     x         -> x
      x        -> x
-- Applies a parser once or more times
  some :: Parser str err a -> Parser str err [a]
  some p = (:)
    <$> p
    <*> many p
-- Applies a parser zero or more times
  many :: Parser str err a -> Parser str err [a]
  many p = Parser $ \s ->
    case runParser (some p) s of
      Left err -> runParser (success []) s
      x        -> x


instance Monad (Parser str err) where
  return :: ok -> Parser str err ok
  return = success
-- Monadic sequence combinator
  (>>=) :: Parser str err a -> (a -> Parser str err b) -> Parser str err b
  p >>= q = Parser $ \s ->
    case runParser p s of
      Left err      -> Left err
      Right (s', a) -> runParser (q a) s'


-- Default sequence combinator
-- If the first parser succeedes then the second parser is used
-- If the first does not succeed then the second one is never tried
-- The result is collected into a pair
seq :: Parser str err a -> Parser str err b -> Parser str err (a, b)
p `seq` q = (,)
  <$> p
  <*> q

--TRIE for keeping lists
data Trie a = Leaf | Branch a [Trie a] | Trie [Trie a]
  deriving (Show, Eq)

splitSymb :: (Eq a) => [[a]] -> [[[a]]]
splitSymb []                    = []
splitSymb ([] : _)              = error "splitSymb" -- undefined
splitSymb list@((symb : _) : _) =
  let (res, rest) = partition (\(x : _) -> x == symb) list
   in res : splitSymb rest

getPrefix :: (Eq a) => [[a]] -> ([a], [[a]])
getPrefix []   = error "getPrefix" -- undefined
getPrefix list =
  let prefix = [head . head $ list]
      rest   = tail <$> list
   in if not $ any null rest
        then if (== 1) . length . splitSymb $ rest
               then let (prefix', rest') = getPrefix rest in (prefix ++ prefix', rest')
               else (prefix, rest)
        else (prefix, rest)

str2trie :: (Eq a, Ord a) => [[a]] -> [Trie [a]]
str2trie = fmap go . splitSymb . sort
  where
    go :: (Eq a, Ord a) => [[a]] -> Trie [a]
    go []   = Leaf
    go [x]  = Branch x [Leaf]
    go list =
      let (prefix, rest) = getPrefix list
          rest'          = filter (not . null) rest
          trieList       = fmap go . splitSymb $ rest'
       in if not $ any null rest
            then Branch prefix trieList
            else Branch prefix (Leaf : trieList)


-- Parses one level and return the first success branch 
parseBranch :: [Trie String] -> Parser Char ErrorType ([Trie String], String)
parseBranch []                   = failP EmptyTrieKeywords
parseBranch (Leaf : xs)          = ([],) <$> success ""
parseBranch (Branch x trie : xs) = ((trie,) <$> tokList x) <|> parseBranch xs

-- Parses keywords
keywords :: [String] -> Parser Char ErrorType String
keywords []  = empty
keywords kws = go . str2trie $ kws
  where
    go :: [Trie String] -> Parser Char ErrorType String
    go trie = Parser $ \s ->
      case runParser (parseBranch trie) s of
        Left err                 -> Left err
        Right (s', ([],    res)) -> Right (s', res)
        Right (s', (trie', res)) -> fmap (res ++) <$> runParser (go trie') s'

-- Checks if the first element of the input is the given token
token :: Eq token => token -> Parser token ErrorType (Batch token)
token t = Parser $
  \case
    []        -> Left [(EmptyInput, (0, 0))]
    (t' : s') -> if input t' == t
                   then Right (s', t')
                   else Left [(ErrorToken, holder t')]

-- Checks if the first part of the input is the given list of tokens
tokList :: (Eq token) => [token] -> Parser token ErrorType [token]
tokList list = Parser $ \s -> go list s
    where
      go []       s        = Right (s, list)
      go _        []       = Left [(FailTokenList, (-1, -1))]
      go (x : xs) (y : ys) = if x == input y
                               then go xs ys
                               else Left [(FailTokenList, holder y)]

-- Checks the predicate
like :: (a -> Bool) -> Parser a ErrorType a
like p = Parser $
  \case
    []       -> Left [(EmptyInput, (0, 0))]
    (x : xs) -> if p . input $ x
                  then Right (xs, input x)
                  else Left [(FailPredicate, holder x)]


-- Negate the result of a parser
notParser :: Parser a ErrorType a -> Parser a ErrorType ()
notParser pars = Parser $ \s ->
  case runParser pars s of
    Left _  -> Right (s, ())
    Right _ -> runParser empty s

