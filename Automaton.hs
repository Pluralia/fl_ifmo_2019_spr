module Automaton where

import qualified Data.Map.Lazy as Map
import qualified Data.Set as Set
import Data.Char (isSpace, isDigit)
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
parseAutomaton :: String -> Maybe (Automaton Symb State)
parseAutomaton = undefined

-----------------------------------------------------------------------------------------------------
--
parseList :: Parser String e -> -- elem
             Parser String d -> -- delim
             Parser String l -> -- lbr
             Parser String r -> -- rbr
             Int             -> -- minimumNumberElems
             Parser String [e]
parseList elem delim lbr rbr minNumElems = fmap (checker minNumElems) $ (:)
  <$> parseBlock
  <*> many (delim *> parseBlock)
  where
    parseBlock = parseSpaces
       *> some lbr
       *> parseSpaces
       *> elem
      <*  parseSpaces
      <*  some rbr
      <*  parseSpaces
    checker :: Int -> [e] -> [e]
    checker n list
      | length list < n = []
      | otherwise       = list

parseSpaces :: Parser String String
parseSpaces = many (like isSpace)


-- TEST PARSERS -------------------------------------------------------------------------------------

mainParser elem n = parseList elem parseDelim parseLbr parseRbr n


type Symb  = Char
type State = Int
data Delta = Delta State State Symb
  deriving (Show, Eq, Ord)

parseDelim :: Parser String Symb
parseDelim = like (== ',')

parseLbr :: Parser String Symb
parseLbr = like (== '<')

parseRbr :: Parser String Symb
parseRbr = like (== '>')

parseState :: Parser String Int
parseState = read <$> some (like isDigit)

parseSymb :: Parser String Symb
parseSymb = like (`elem` ['a'..'z'] ++ ['A'..'Z'])

parseDelta :: Parser String Delta
parseDelta = Delta <$>
      parseState
  <*  parseDelim
  <*  parseSpaces
  <*> parseState
  <*  parseDelim
  <*  parseSpaces
  <*> parseSymb
