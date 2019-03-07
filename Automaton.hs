module Automaton where

import qualified Data.Map.Lazy as Map
import qualified Data.Set as Set
import Data.Char (isSpace, isDigit)
import Data.Maybe (catMaybes)
import Data.Bifunctor (first)
import Combinators

type Set = Set.Set
type Map = Map.Map

data Automaton s q = Automaton { sigma     :: Set s
                               , states    :: Set q
                               , initState :: q
                               , termState :: Set q
                               , delta     :: Map (q, s) (Set q)
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
      | not $ initState auto `Set.member` states auto = lr InitStNotSt
      | full $ termState auto Set.\\ states auto      = lr TermStNotSt
      | full $ usedStatesTo auto Set.\\ states auto   = lr DeltaOnNotSt
      | full $ usedStatesFrom auto Set.\\ states auto = lr DeltaOnNotSt
      | full $ usedSymbols auto Set.\\ sigma auto     = lr DeltaOnNotSigm
      | otherwise                                     = Right auto

usedStatesFrom :: Automaton Symb State -> Set State
usedStatesFrom = Set.fromList . fmap fst . Map.keys . delta

usedStatesTo :: Automaton Symb State -> Set State
usedStatesTo = Set.unions . Map.elems . delta

usedSymbols :: Automaton Symb State -> Set Symb
usedSymbols = Set.fromList . snd . unzip . Map.keys . delta


parseAutomaton' :: Parser Char ErrorType (Automaton Symb State)
parseAutomaton' = Automaton
  <$> (Set.fromList <$> mainParser parseSymb 0)
  <*> (Set.fromList <$> mainParser parseState 1)
  <*> (head <$> mainParser parseState 1)
  <*> (Set.fromList <$> mainParser parseState 0)
  <*> (Map.fromAscListWithKey concatStTo . (fmap formatDelta) <$> mainParser parseDelta 0)
  <*  notParser (like (const True))

formatDelta :: Delta -> ((State, Symb), Set State)
formatDelta (Delta st1 symb st2)
  | st1 == st2 = ((st1, symb), Set.empty)
  | otherwise  = ((st1, symb), Set.singleton $ st2)

concatStTo :: (State, Symb) -> Set State -> Set State -> Set State
concatStTo (fromSt, _) toSt1 toSt2 =
  let st1 = if null toSt1 then Set.singleton fromSt else toSt1
      st2 = if null toSt2 then Set.singleton fromSt else toSt2
   in st1 `Set.union` st2

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

-- Checks if the automaton is deterministic 
-- (only one transition for each state and each input symbol)
isDFA :: Automaton Symb State -> Bool
isDFA auto =
  let noMultTrans = null . filter (> 1) . fmap Set.size . Map.elems . delta $ auto
      noEpsTrans  = null . filter (== "epsilon") . fmap snd . Map.keys . delta $ auto
   in noMultTrans && noEpsTrans

-- Checks if the automaton is nondeterministic
-- (eps-transition or multiple transitions for a state and a symbol)
isNFA :: Automaton Symb State -> Bool
isNFA = not . isDFA

-- Checks if the automaton is complete 
-- (there exists a transition for each state and each input symbol)
isComplete :: Automaton Symb State -> Bool 
isComplete auto =
  let noUnusedStates = null $ states auto Set.\\ (usedStatesFrom auto `Set.union` usedStatesTo auto)
      noUnusedSigma  = null $ sigma auto Set.\\ usedSymbols auto
   in noUnusedStates && noUnusedSigma

-- Checks if the automaton is minimal 
-- (only for DFAs: the number of states is minimal)
isMinimal :: Automaton Symb State -> Bool
isMinimal auto | isNFA auto || not (isComplete auto) || existAloneSt auto = False
isMinimal auto =
  let revDelta     = reverseDelta auto
      notTermStSet = states auto Set.\\ termState auto
      initQueue    = Set.toList $ notTermStSet `Set.cartesianProduct` termState auto
   in equiveClasses auto initQueue 

existAloneSt :: Automaton Symb State -> Bool
existAloneSt auto = null $ states auto Set.\\ stFromInit
  where
    deltaInfo = Map.fromListWith (Set.union) . fmap (first fst) . Map.toList . delta $ auto
    stFromInit = dfs [initState auto] (Set.singleton . initState $ auto) deltaInfo
    dfs :: [State] -> Set State -> Map State (Set State) -> Set State
    dfs []       res _    = res
    dfs (x : xs) res info = dfs xs (maybe res (Set.union res) $ Map.lookup x info) info

reverseDelta :: Automaton Symb State -> Map (State, Symb) (Set State)
reverseDelta = Map.fromAscListWithKey concatStTo . concatMap go . Map.toList . delta
  where
    go :: ((q, s), Set q) -> [((q, s), Set q)]
    go res@((fromSt, symb), toStSet)
      | null toStSet = [res]
      | otherwise    = (\toSt -> ((toSt, symb), Set.singleton fromSt)) <$> Set.toList toStSet

--equiveClasses :: Automaton Symb State -> [(State, State)] -> _
--equiveClasses _    []       acc = acc
equiveClasses auto (x : xs) = undefined
