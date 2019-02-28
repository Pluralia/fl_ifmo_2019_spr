module Automaton where

import qualified Data.Map.Lazy as Map
import qualified Data.Set as Set
import Data.Char (isSpace, isDigit)
import Combinators
import Data.Maybe (catMaybes)

type Set = Set.Set
type Map = Map.Map

data Automaton s q = Automaton { sigma     :: Set s
                               , states    :: Set q
                               , initState :: q
                               , termState :: Set q
                               , delta     :: Map (q, s) (Maybe q)
                               }
  deriving (Show)

-- Top level function: parses input string, checks that it is an automaton, and then returns it.
-- Should return Nothing, if there is a syntax error or the automaton is not a correct automaton.
-- This includes:
-- * The set of states is empty
-- * The init state is not a state
-- * Any of the terminal states is not a state
-- * Delta function is defined on not-a-state or not-a-symbol-from-sigma
parseAutomaton :: String -> Maybe (Automaton Symb State)
parseAutomaton = checkAutomaton . checkRest $ runParser $ Automaton <$>
      (Set.fromList <$> mainParser parseSymb 0)
  <*> (Set.fromList <$> mainParser parseState 1)
  <*> (head <$> mainParser parseState 1)
  <*> (Set.fromList <$> mainParser parseState 0)
  <*> (Map.fromList . (fmap formatDelta) <$> mainParser parseDelta 0)

formatDelta :: Delta -> ((State, Symb), Maybe State)
formatDelta (Delta st1 symb st2)
  | st1 == st2 = ((st1, symb), Nothing)
  | otherwise  = ((st1, symb), Just st2)

checkRest :: (String -> Maybe (String, Automaton Symb State)) ->
             (String -> Maybe (Automaton Symb State))
checkRest pars = \str ->
  case pars str of
    Just ("", res) -> Just res
    _              -> Nothing

checkAutomaton :: (String -> Maybe (Automaton Symb State)) ->
                  (String -> Maybe (Automaton Symb State))
checkAutomaton getAuto = maybe Nothing go . getAuto
  where
    go :: Automaton Symb State -> Maybe (Automaton Symb State)
    go auto
      | not $ initState auto `Set.member` states auto                                        = Nothing
      | not . null $ termState auto Set.\\ states auto                                       = Nothing
      | not . null $ (Set.fromList . catMaybes . Map.elems $ delta auto) Set.\\ states auto  = Nothing
      | not . null $ (Set.fromList . fst . unzip . Map.keys $ delta auto) Set.\\ states auto = Nothing
      | not . null $ (Set.fromList . snd . unzip . Map.keys $ delta auto) Set.\\ sigma auto  = Nothing
      | otherwise = Just auto

-----------------------------------------------------------------------------------------------------

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

mainParser elem n = parseSpaces
   *> some parseTriLbr
   *> parseList elem parseDelim parseLbr parseRbr n
  <*  some parseTriRbr
  <*  parseSpaces

type Symb  = Char
type State = Int
data Delta = Delta State Symb State
  deriving (Show, Eq, Ord)

parseDelim :: Parser String Symb
parseDelim = like (== ',')

parseTriLbr :: Parser String Symb
parseTriLbr = like (== '<')

parseTriRbr :: Parser String Symb
parseTriRbr = like (== '>')

parseLbr :: Parser String Symb
parseLbr = like (== '(')

parseRbr :: Parser String Symb
parseRbr = like (== ')')

parseState :: Parser String Int
parseState = read <$> some (like isDigit)

parseSymb :: Parser String Symb
parseSymb = like (`elem` ['a'..'z'] ++ ['A'..'Z'])

parseDelta :: Parser String Delta
parseDelta = Delta <$>
      parseState
  <*  parseDelim
  <*  parseSpaces
  <*> parseSymb
  <*  parseDelim
  <*  parseSpaces
  <*> parseState

