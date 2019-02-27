module Automaton where

import qualified Data.Map.Lazy as Map
import qualified Data.Set as Set
import Data.Char (isSpace)
import Combinators

type Set = Set.Set
type Map = Map.Map

data Automaton s q = Automaton { sigma     :: Set s
                               , states    :: Set q
                               , initState :: q
                               , termState :: Set q
                               , delta     :: Map (q, s) (Maybe q)
                               }

-- Top level function: parses input string, checks that it is an automaton, and then returns it.
-- Should return Nothing, if there is a syntax error or the automaton is not a correct automaton.
-- This includes:
-- * The set of states is empty
-- * The init state is not a state
-- * Any of the terminal states is not a state
-- * Delta function is defined on not-a-state or not-a-symbol-from-sigma
-- Pick appropriate types for s and q
-- parseAutomaton :: String -> Maybe (Automaton ? ?)

parseAutomaton = undefined

data ELement = Delta | Char | State
  deriving (Show, Eq, Ord)

parseList :: Parser String e -> -- elem
             Parser String d -> -- delim
             Parser String l -> -- lbr
             Parser String r -> -- rbr
             Int             -> -- minimumNumberElems
             Parser String [e]
parseList elem delim lbr rbr minNumElems = fmap (checker minNumElems) . many $
      many (like isSpace)
   *> some lbr
   *> many (like isSpace)
   *> elem
  <*  many (like isSpace)
  <*  some rbr
  <*  many (like isSpace)
  where
    checker :: Int -> [e] -> [e]
    checker n list
      | length list < n = []
      | otherwise       = list
