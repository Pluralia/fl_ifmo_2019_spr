module Automaton where

import qualified Data.Map.Lazy as Map
import qualified Data.Set as Set
import Data.Char (isSpace, isDigit)
import Data.Maybe (catMaybes)
import Combinators

type Set = Set.Set
type Map = Map.Map

data Automaton s q = Automaton { sigma     :: Set s
                               , states    :: Set q
                               , initState :: q
                               , termState :: Set q
                               , delta     :: Map (q, s) (Maybe q)
                               }
  deriving (Show)

data AutomatonError = InitStNotSt
                    | TermStNotSt
                    | DeltaOnNotSt
                    | DeltaOnNotSigm
  deriving (Show)

-- Top level function: parses input string, checks that it is an automaton, and then returns it.
-- Should return Nothing, if there is a syntax error or the automaton is not a correct automaton.
-- This includes:
-- * The set of states is empty
-- * The init state is not a state
-- * Any of the terminal states is not a state
-- * Delta function is defined on not-a-state or not-a-symbol-from-sigma
parseAutomaton :: String -> Either String (Automaton Symb State)
parseAutomaton =
  either (Left . err2str) Right . checkAutomaton . runParser parseAutomaton' . str2Batch

str2Batch :: String -> [Batch Char]
str2Batch input = concat $
  (\(l, symbs) -> (\(n, symb) -> Batch symb (l, n)) <$> zip [1..] symbs) <$> zip [1..] (lines input)

err2str :: Either [(ErrorType, Holder)] AutomatonError -> String
err2str = either (concatMap errType2str) autoErr2str
  where
    errType2str :: (ErrorType, Holder) -> String
    errType2str (err, (l, n)) = show err ++ ":" ++ show l ++ ":" ++ show n ++ "\n"
    autoErr2str :: AutomatonError -> String
    autoErr2str InitStNotSt    = "The init state is not a state.\n"
    autoErr2str TermStNotSt    = "Any of the terminal states is not a state.\n"
    autoErr2str DeltaOnNotSt   = "Delta function is defined on not-a-state.\n"
    autoErr2str DeltaOnNotSigm = "Delta function is defined on not-a-symbol-from-sigma.\n"

checkAutomaton :: Either [(ErrorType, Holder)] ([Batch Char], Automaton Symb State) ->
                  Either (Either [(ErrorType, Holder)] AutomatonError) (Automaton Symb State)
checkAutomaton = either (Left . Left) (go . snd)
  where
    full = not . null
    lr = Left . Right
    go :: Automaton Symb State ->
          Either (Either [(ErrorType, Holder)] AutomatonError) (Automaton Symb State)
    go auto
      | not $ initState auto `Set.member` states auto                                  =
          lr InitStNotSt
      | full $ termState auto Set.\\ states auto                                       =
          lr TermStNotSt
      | full $ (Set.fromList . catMaybes . Map.elems $ delta auto) Set.\\ states auto  =
          lr DeltaOnNotSt
      | full $ (Set.fromList . fst . unzip . Map.keys $ delta auto) Set.\\ states auto =
          lr DeltaOnNotSt
      | full $ (Set.fromList . snd . unzip . Map.keys $ delta auto) Set.\\ sigma auto  =
          lr DeltaOnNotSigm
      | otherwise = Right auto

parseAutomaton' :: Parser Char ErrorType (Automaton Symb State)
parseAutomaton' = Automaton
  <$> (Set.fromList <$> mainParser parseSymb 0)
  <*> (Set.fromList <$> mainParser parseState 1)
  <*> (head <$> mainParser parseState 1)
  <*> (Set.fromList <$> mainParser parseState 0)
  <*> (Map.fromList . (fmap formatDelta) <$> mainParser parseDelta 0)
  <*  notParser (like (const True))

formatDelta :: Delta -> ((State, Symb), Maybe State)
formatDelta (Delta st1 symb st2)
  | st1 == st2 = ((st1, symb), Nothing)
  | otherwise  = ((st1, symb), Just st2)

-----------------------------------------------------------------------------------------------------

parseList :: Parser Char ErrorType e -> -- elem
             Parser Char ErrorType d -> -- delim
             Parser Char ErrorType l -> -- lbr
             Parser Char ErrorType r -> -- rbr
             Int                     -> -- minimumNumberElems
             Parser Char ErrorType [e]
parseList elem delim lbr rbr minNumElems = fmap (checker minNumElems) $
      ((:)
  <$> parseBlock
  <*> many (delim *> parseBlock)) <|> success []
  where
    parseBlock = parseSpaces
       *> lbr
       *> parseSpaces
       *> elem
      <*  parseSpaces
      <*  rbr
      <*  parseSpaces
    checker :: Int -> [e] -> [e]
    checker n list
      | length list < n = []
      | otherwise       = list


-- TEST PARSERS -------------------------------------------------------------------------------------

mainParser elem n = parseSpaces
   *> some parseTriLbr
   *> parseList elem parseDelim parseLbr parseRbr n
  <*  some parseTriRbr
  <*  parseSpaces

type Symb  = String
type State = String
data Delta = Delta State Symb State
  deriving (Show, Eq, Ord)

parseDelim :: Parser Char ErrorType Char
parseDelim = like (== ',')

parseTriLbr :: Parser Char ErrorType Char
parseTriLbr = like (== '<')

parseTriRbr :: Parser Char ErrorType Char
parseTriRbr = like (== '>')

parseLbr :: Parser Char ErrorType Char
parseLbr = like (== '(')

parseRbr :: Parser Char ErrorType Char
parseRbr = like (== ')')

parseState :: Parser Char ErrorType State
parseState = some $ like (`elem` ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'])

parseSymb :: Parser Char ErrorType Symb
parseSymb = some $ like (`elem` ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'])

parseSpaces :: Parser Char ErrorType String
parseSpaces = many (like isSpace)

parseDelta :: Parser Char ErrorType Delta
parseDelta = Delta <$>
      parseState
  <*  parseDelim
  <*  parseSpaces
  <*> parseSymb
  <*  parseDelim
  <*  parseSpaces
  <*> parseState

-----------------------------------------------------------------------------------------------------

-- Checks if the automaton is deterministic (only one transition for each state and each input symbol)
isDFA :: Automaton a b -> Bool
isDFA = undefined

-- Checks if the automaton is nondeterministic (eps-transition or multiple transitions for a state and a symbol)
isNFA :: Automaton a b -> Bool
isNFA = undefined

-- Checks if the automaton is complete (there exists a transition for each state and each input symbol)
isComplete :: Automaton a b -> Bool 
isComplete = undefined

-- Checks if the automaton is minimal (only for DFAs: the number of states is minimal)
isMinimal :: Automaton a b -> Bool
isMinimal = undefined


