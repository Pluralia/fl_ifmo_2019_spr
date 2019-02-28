{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TupleSections #-}

module Combinators
    ( Parser(..)  
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


-- Parsing result is some payload and a suffix of the input which is yet to be parsed
newtype Parser str ok = Parser { runParser :: str -> Maybe (str, ok) }

-- Parser which always succeedes consuming no input
success :: ok -> Parser str ok
success ok = Parser $ \s -> Just (s, ok)

-- Parser which fails no mater the input
failP :: Parser str ok
failP = Parser $ const Nothing


instance Functor (Parser str) where
-- Applies a function to the parsing result, if parser succeedes
  fmap :: (a -> b) -> Parser str a -> Parser str b
  fmap f p = Parser $ \s ->
    case runParser p s of
      Nothing      -> Nothing
      Just (s', a) -> Just (s', f a)


instance Applicative (Parser str) where
  pure :: a -> Parser str a
  pure = success
-- Applicative sequence combinator
  (<*>) :: Parser str (a -> b) -> Parser str a -> Parser str b
  p <*> q = Parser $ \s ->
    case runParser p s of
      Nothing      -> Nothing        
      Just (s', f) -> runParser (f <$> q) s'


instance Alternative (Parser str) where
  empty :: Parser str ok
  empty = failP
-- Biased choice: if the first parser succeedes, the second is never run
  (<|>) :: Parser str ok -> Parser str ok -> Parser str ok
  p <|> q = Parser $ \s ->
    case runParser p s of
      Nothing -> runParser q s
      x -> x
-- Applies a parser once or more times
  some :: Parser str a -> Parser str [a]
  some p = (:)
    <$> p
    <*> many p
-- Applies a parser zero or more times
  many :: Parser str a -> Parser str [a]
  many p = Parser $ \s ->
    case runParser (some p) s of
      Nothing      -> runParser (success []) s
      x            -> x      


instance Monad (Parser str) where
  return :: a -> Parser str a
  return = success
-- Monadic sequence combinator
  (>>=) :: Parser str a -> (a -> Parser str b) -> Parser str b
  p >>= q = Parser $ \s ->
    case runParser p s of
      Nothing      -> Nothing
      Just (s', a) -> runParser (q a) s'


-- Default sequence combinator
-- If the first parser succeedes then the second parser is used
-- If the first does not succeed then the second one is never tried
-- The result is collected into a pair
seq :: Parser str a -> Parser str b -> Parser str (a, b)
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
   in if null $ filter null rest
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
       in if null $ filter null rest
            then Branch prefix trieList
            else Branch prefix (Leaf : trieList)


-- Parses one level and return the first success branch 
parseBranch :: [Trie String] -> Parser String ([Trie String], String)
parseBranch []                     = empty
parseBranch (Leaf : xs)            = ([],) <$> success ""
parseBranch ((Branch x trie) : xs) = ((trie,) <$> tokList x) <|> parseBranch xs

-- Parses keywords
keywords :: [String] -> Parser String String
keywords []  = empty
keywords kws = go . str2trie $ kws
  where
    go :: [Trie String] -> Parser String String
    go trie = Parser $ \s ->
      case runParser (parseBranch trie) s of
        Nothing                 -> Nothing
        Just (s', ([], res))    -> Just (s', res)
        Just (s', (trie', res)) -> fmap (res ++) <$> runParser (go trie') s'

-- Checks if the first element of the input is the given token
token :: Eq token => token -> Parser [token] token
token t = Parser $ \s ->
  case s of
    (t' : s') | t == t' -> Just (s', t)
    _                   -> Nothing

-- Checks if the first part of the input is the given list of tokens
tokList :: (Eq a) => [a] -> Parser [a] [a]
tokList list = Parser $ \s -> go list s
    where
      go []       s        = Just (s, list)
      go _        []       = Nothing
      go (x : xs) (y : ys) = if x == y then go xs ys else Nothing

-- Checks the predicate
like :: (a -> Bool) -> Parser [a] a
like p = Parser go
  where
    go [] = Nothing
    go (x : xs) | p x       = Just (xs, x)
                | otherwise = Nothing

-- Negate the result of a parser
notParser :: Parser String a -> Parser String ()
notParser pars = Parser $ \str ->
  case runParser pars str of
    Nothing -> Just (str, ())
    Just _  -> Nothing

