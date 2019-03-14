module Automaton where

import qualified Data.Map.Lazy as Map
import qualified Data.Set      as Set
import           Data.Char      (isSpace, isDigit)
import           Data.Maybe     (catMaybes)
import           Data.Bifunctor (bimap, first, second)
import           Data.Tuple     (swap)
import           Combinators

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
                --  | isNFA
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
      | not $ initState auto `Set.member` states auto                              = lr InitStNotSt
      | full $ termState auto Set.\\ states auto                                   = lr TermStNotSt
      | full $ usedStatesTo auto Set.\\ states auto                                = lr DeltaOnNotSt
      | full $ usedStatesFrom auto Set.\\ states auto                              = lr DeltaOnNotSt
      | full $ usedSymbols auto Set.\\ sigma auto Set.\\ Set.singleton "\\epsilon" = lr DeltaOnNotSigm
      | otherwise                                                                  = Right auto

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
formatDelta (Delta st1 symb st2) = ((st1, symb), Set.singleton $ st2)

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
parseState = some $ like (`elem` '\\' : ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'])

parseSymb :: Parser Char ErrorType Symb
parseSymb = some $ like (`elem` '\\' : ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'])

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
  let applyUnion = Map.fromListWith Set.union
      noMultTrans = and . fmap ((< 2) . Set.size) . Map.elems . applyUnion . Map.toList . delta $ auto
      noEpsTrans  = not $ "\\epsilon" `Set.member` usedSymbols auto
   in noMultTrans && noEpsTrans


-- Checks if the automaton is nondeterministic
-- (eps-transition or multiple transitions for a state and a symbol)
isNFA :: Automaton Symb State -> Bool
isNFA = const True


-- Checks if the automaton is complete
-- (there exists a transition for each state and each input symbol)
isComplete :: Automaton Symb State -> Bool 
isComplete auto = noAloneSt && noAloneSymb
  where
    noAloneSt    = null $ existAloneSt auto
    
    symbsFromSts = Map.fromListWith Set.union . fmap (second Set.singleton) .  Map.keys . delta $ auto
    noAloneSymb  = and . fmap (null . (sigma auto Set.\\)) . Map.elems $ symbsFromSts

existAloneSt :: Automaton Symb State -> Set State
existAloneSt auto =states auto Set.\\ (dfs Set.empty startSt startSt)
  where
    startSt = Set.singleton . initState $ auto
    
    deltaInfo :: Map State (Set State)
    deltaInfo = Map.fromListWith Set.union . fmap (first fst) . Map.toList . delta $ auto
    --    visitedSts -> startSts -> reachableSts -> newReachableSts
    dfs :: Set State -> Set State -> Set State -> Set State
    dfs started starts acc
      | null starts = acc
      | (from, newStarts) <- Set.deleteFindMin starts
      , False             <- from `Set.member` started
      , Just toSts        <- Map.lookup from deltaInfo
          = dfs (Set.insert from started) (newStarts `Set.union` toSts) (acc `Set.union` toSts)
      | otherwise   = dfs started (snd . Set.deleteFindMin $ starts) acc


-- Checks if the automaton is minimal 
-- (only for DFAs: the number of states is minimal)
isMinimal :: Automaton Symb State -> Bool
isMinimal auto
  | isDFA auto && isComplete auto
      = null $ (Set.fromList . equiveClasses $ auto) Set.\\ (Set.map Set.singleton . states $ auto)
  | otherwise = False


equiveClasses :: Automaton Symb State -> [Set State]
equiveClasses auto = getClasses (states auto) $ go (initQueue auto) (Set.fromList $ initQueue auto)
  where
    revDelta = reverseDelta auto
    -- queue -> visited_pairs -> class_table
    go :: [(State, State)] -> Set (State, State) -> Set (State, State)
    go []    _       = Set.empty
    go queue visited
      | pairsFromQueue <- Set.unions $ getPairsBy (Set.toList . sigma $ auto) revDelta <$> queue
      , newPairs       <- pairsFromQueue Set.\\ visited
      , currClassTable <- pairsFromQueue `Set.union` visited
          = currClassTable `Set.union` (go (Set.toList $ newPairs) currClassTable)


-- sigmas -> reverse_delta -> one_pair_from_queue -> new_pairs_to_queue
getPairsBy :: [Symb] -> Map (State, Symb) (Set State) -> (State, State) -> Set (State, State)
getPairsBy []         _        _             = Set.empty
getPairsBy s@(x : xs) revDelta el@(from, to)
  | Just fromSet <- Map.lookup (from, x) revDelta
  , Just toSet   <- Map.lookup (to, x) revDelta
      = (fromSet `Set.cartesianProduct` toSet) `Set.union` (getPairsBy xs revDelta el)
  | otherwise = getPairsBy xs revDelta el


getClasses :: Set State -> Set (State, State) -> [Set State]
getClasses allStates classTable = classes
  where
    applyUnion  = Map.fromListWith Set.union
    
    fromStToSet :: Map (Set State) (Set State)
    fromStToSet = applyUnion . fmap (bimap Set.singleton Set.singleton) . Set.toList $ classTable
    
    classes :: [Set State]
    classes =
      let partOfClasses = Map.elems . applyUnion . fmap swap . Map.toList $ fromStToSet 
          plusClass = allStates Set.\\ Set.unions partOfClasses
       in if null plusClass then partOfClasses else plusClass : partOfClasses


initQueue :: Automaton Symb State -> [(State, State)]
initQueue auto = Set.toList $ notTermStSet `Set.cartesianProduct` termState auto
  where
    notTermStSet = states auto Set.\\ termState auto

    
reverseDelta :: Automaton Symb State -> Map (State, Symb) (Set State)
reverseDelta = Map.fromListWith Set.union . concatMap go . Map.toList . delta
  where
    go :: ((q, s), Set q) -> [((q, s), Set q)]
    go ((fromSt, symb), toStSet) = 
      (\toSt -> ((toSt, symb), Set.singleton fromSt)) <$> Set.toList toStSet


-- return epsilon-close of the automaton
epsilonClose :: Automaton Symb State -> Automaton Symb State
epsilonClose = id


-- return the DFA
toDFA :: Automaton Symb State -> Automaton Symb State
toDFA = id


-- return minimal automaton if input is the DFA
doMinimal :: Automaton Symb State -> Either String (Automaton Symb State)
doMinimal auto
  | isNFA auto = Left "isNFA"
  | isDFA auto = (doComplete . removeAloneSt $ auto) >>= Right . minimize
  where
    minimize :: Automaton Symb State -> Automaton Symb State
    minimize auto = auto -- equiveClasses auto


removeAloneSt :: Automaton Symb State -> Automaton Symb State
removeAloneSt auto = Automaton
  (sigma auto)
  (states auto Set.\\ exSt)
  (initState auto)
  (termState auto Set.\\ exSt)
  (removeToSt . removeFromSt . delta $ auto)
  where
    exSt         = existAloneSt auto
    removeFromSt =
      Map.filterWithKey (\(from, symb) to -> not $ from `Set.member` exSt) 
    removeToSt   =
      Map.mapMaybe (\toSt -> if not . null $ exSt `Set.intersection` toSt then Nothing else Just toSt)


-- return complete automaton
doComplete :: Automaton Symb State -> Either String (Automaton Symb State)
doComplete auto
  | isNFA auto = Left "isNFA"
  | isDFA auto = Right $ Automaton
  (sigma auto)
  ("bottom" `Set.insert` states auto)
  (initState auto)
  (termState auto)
  (addDeltas (delta auto) (Set.toList . states $ auto))
  where
    usedSymbForCurrSt :: State -> [Symb]
    usedSymbForCurrSt st = fmap snd . filter (\(from, _) -> from == st) . Map.keys . delta $ auto

    unusedSymbForCurrSt :: State -> [Symb]
    unusedSymbForCurrSt = Set.toList . (sigma auto Set.\\) . Set.fromList . usedSymbForCurrSt
  
    addDeltasSymb :: Map (State, Symb) (Set State) -> [Symb] -> State -> Map (State, Symb) (Set State)
    addDeltasSymb delt []          _  = delt
    addDeltasSymb delt (symb : xs) st =
      addDeltasSymb (Map.insert (st, symb) (Set.singleton "bottom") delt) xs st

    addDeltas :: Map (State, Symb) (Set State) -> [State] -> Map (State, Symb) (Set State)
    addDeltas delt []        = delt
    addDeltas delt (st : xs) =
      addDeltas (addDeltasSymb delt (unusedSymbForCurrSt st) st) xs



