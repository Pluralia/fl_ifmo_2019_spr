{-# LANGUAGE TupleSections #-}

module Lib
    ( isDFA
    , isNFA
    , isComplete
    , isMinimal
    , doComplete
    , minimize
    , toDFA
    , epsClosure
    ) where

import qualified Data.Map.Lazy as Map
import qualified Data.Set      as Set
import           Data.Char      (isSpace, isDigit)
import           Data.Maybe     (catMaybes)
import           Data.Bifunctor (bimap, first, second)
import           Data.Tuple     (swap)
import           Automaton

type Set = Set.Set
type Map = Map.Map


----------------------------------------------------------------------------------------------------
-- Checks if the automaton is deterministic 
-- (only one transition for each state and each input symbol)
isDFA :: Automaton Symb State -> Bool
isDFA auto =
  let applyUnion = Map.fromListWith Set.union
      noMultTrans = and . fmap ((< 2) . Set.size) . Map.elems . applyUnion . Map.toList . delta $ auto
      noEpsTrans  = not $ "\\epsilon" `Set.member` usedSymbols auto
   in noMultTrans && noEpsTrans


----------------------------------------------------------------------------------------------------
-- Checks if the automaton is nondeterministic
-- (eps-transition or multiple transitions for a state and a symbol)
isNFA :: Automaton Symb State -> Bool
isNFA = const True


----------------------------------------------------------------------------------------------------
-- Checks if the automaton is complete
-- (there exists a transition for each state and each input symbol)
isComplete :: Automaton Symb State -> Bool 
isComplete auto = isDFA auto && noAloneSymb
  where
    noAloneSymb = and . fmap (null . (sigma auto Set.\\)) . Map.elems . symbsByEverySt $ auto


symbsByEverySt :: Automaton Symb State -> Map State (Set Symb)
symbsByEverySt auto = stSymbDelta `Map.union` addStSymb
  where
    stSymbDelta = Map.fromListWith Set.union . fmap (second Set.singleton) .  Map.keys . delta $ auto
    stFromDelta = Set.difference (states auto) . Set.fromList . Map.keys $ stSymbDelta
    addStSymb   = Map.fromList . fmap (, Set.empty) . Set.toList $ stFromDelta


availableSt :: Automaton Symb State -> Set State
availableSt auto = dfs Set.empty startSt startSt
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


----------------------------------------------------------------------------------------------------
-- Checks if the automaton is minimal 
-- (only for DFAs: the number of states is minimal)
isMinimal :: Automaton Symb State -> Bool
isMinimal auto = case doComplete auto of
  Left  _       -> False
  Right comAuto ->
    null $ (Set.fromList . equiveClasses $ comAuto) Set.\\ (Set.map Set.singleton . states $ comAuto)


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
          = currClassTable `Set.union` go (Set.toList newPairs) currClassTable

    initQueue :: Automaton Symb State -> [(State, State)]
    initQueue auto = Set.toList $ notTermStSet `Set.cartesianProduct` termState auto
      where
        notTermStSet = states auto Set.\\ termState auto


-- sigmas -> reverse_delta -> one_pair_from_queue -> new_pairs_to_queue
getPairsBy :: [Symb] -> Map (State, Symb) (Set State) -> (State, State) -> Set (State, State)
getPairsBy []         _        _             = Set.empty
getPairsBy s@(x : xs) revDelta el@(from, to)
  | Just fromSet <- Map.lookup (from, x) revDelta
  , Just toSet   <- Map.lookup (to, x) revDelta
      = (fromSet `Set.cartesianProduct` toSet) `Set.union` getPairsBy xs revDelta el
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


reverseDelta :: Automaton Symb State -> Map (State, Symb) (Set State)
reverseDelta = Map.fromListWith Set.union . concatMap go . Map.toList . delta
  where
    go :: ((q, s), Set q) -> [((q, s), Set q)]
    go ((fromSt, symb), toStSet) = 
      (\toSt -> ((toSt, symb), Set.singleton fromSt)) <$> Set.toList toStSet


----------------------------------------------------------------------------------------------------
bottom = "bottom"
bottomSet = Set.singleton bottom

-- return complete automaton
doComplete :: Automaton Symb State -> Either String (Automaton Symb State)
doComplete auto
  | isDFA auto && isComplete auto = Right auto
  | isDFA auto                    = Right comAuto
  | otherwise                     = Left "isNFA"
  where
    comAuto = Automaton
      (sigma auto)
      (bottom `Set.insert` states auto)
      (initState auto)
      (termState auto)
      (Map.unions [delta auto, Map.fromList addDelta, Map.fromList bottomDelta])

    transformStSetSymb :: (State, Set Symb) -> [(State, Symb)]
    transformStSetSymb (st, symbSet) = fmap (st,) . Set.toList $ symbSet

    addStSymb = Map.filter (not . Set.null) . Map.map ((Set.\\) (sigma auto)) . symbsByEverySt $ auto

    addDelta = concatMap (fmap (, bottomSet) . transformStSetSymb). Map.toList $ addStSymb

    bottomDelta = fmap (\x -> ((bottom, x), bottomSet)) . Set.toList . sigma $ auto


-----------------------------------------------------------------------------------------------------
-- return minimal automaton
minimize :: Automaton Symb State -> Either String (Automaton Symb State)
minimize auto
  | not $ isDFA auto = Left "isNFA"
  | otherwise        = (\a -> buildAuto a $ equiveClasses a) <$> (doComplete . removeAloneSt $ auto)


buildAuto :: Automaton Symb State -> [Set State] -> Automaton Symb State
buildAuto auto classes = Automaton
  (sigma auto)
  (Set.fromList . fmap union2OneSt $ classes)
  (toNewState . initState $ auto)
  (Set.map toNewState . termState $ auto)
  (Map.fromList . fmap toNewDelta . Map.toList . delta $ auto)
  where
    union2OneSt :: Set State -> State
    union2OneSt = concat . Set.toList
    
    toNewState :: State -> State
    toNewState st = union2OneSt . head . filter (Set.member st) $ classes

    toNewDelta :: ((State, Symb), Set State) -> ((State, Symb), Set State)
    toNewDelta ((from, symb), to) = ((toNewState from, symb), toNewState `Set.map` to)


removeAloneSt :: Automaton Symb State -> Automaton Symb State
removeAloneSt auto = Automaton
  (sigma auto)
  (availableSt auto)
  (initState auto)
  (termState auto Set.\\ aloneSt)
  (removeToSt . removeFromSt . delta $ auto)
  where
    aloneSt      = states auto Set.\\ availableSt auto
    removeFromSt =
      Map.filterWithKey (\(from, symb) to -> not $ from `Set.member` aloneSt) 
    removeToSt   =
      Map.mapMaybe (\toSt -> if null $ aloneSt `Set.intersection` toSt then Just toSt else Nothing)


-----------------------------------------------------------------------------------------------------
-- convert NKA to DKA
toDFA :: Automaton Symb State -> Either String (Automaton Symb State)
toDFA auto
  | isDFA auto                          = Right auto
  | "\\epsilon" `Set.member` sigma auto = Left "There is epsilon"
  | otherwise                           = Right $ resAuto
  where
    resAuto = Automaton
      (sigma auto)
      (union2OneSt `Set.map` usedSt) 
      (initState auto)
      (Set.unions . Set.toList . Set.map getTermState . termState $ auto)
      usedDelta   

    (usedSt, usedDelta) = dfaDelta initQueue (Set.fromList initQueue, Map.empty)
    
    union2OneSt :: Set State -> State
    union2OneSt = concat . Set.toList
    
    getTermState :: State -> Set State
    getTermState st = Set.map union2OneSt . Set.filter (st `Set.member`) $ usedSt

    initQueue :: [Set State] 
    initQueue = [Set.singleton . initState $ auto]

    getNextSet :: [State] -> Symb -> Set State
    getNextSet [] _          = Set.empty
    getNextSet (x : xs) symb = xSts `Set.union` getNextSet xs symb
      where
        xSts = maybe Set.empty id . Map.lookup (x, symb) . delta $ auto

    getDelta :: [State] -> Symb -> ((State, Symb), Set State)
    getDelta sts symb = ((concat sts, symb), Set.singleton . union2OneSt $ getNextSet sts symb)

    symbList :: [Symb]
    symbList = Set.toList . sigma $ auto
    
    -- queue -> used_states -> acc_delta -> delta
    dfaDelta :: [Set State]                   ->
                (Set (Set State), Map (State, Symb) (Set State)) ->
                (Set (Set State), Map (State, Symb) (Set State))
    dfaDelta []       res         = res
    dfaDelta (x : xs) (used, acc) = dfaDelta queue (x `Set.insert` used, xDeltas `Map.union` acc)
      where
        xSts    = Set.unions . fmap (getNextSet (Set.toList x)) $ symbList
        queue   = if xSts `Set.member` used then xs else (xSts : xs)
        xDeltas = Map.fromList . fmap (getDelta (Set.toList x)) $ symbList


-----------------------------------------------------------------------------------------------------
-- return epsilon-closure of the automaton
epsClosure :: Automaton Symb State -> Automaton Symb State
epsClosure auto
  | "\\epsilon" `Set.member` sigma auto = auto
  | otherwise                           = resAuto
  where
    resAuto =
      let Automaton symbSet stateSet init term deltaMap = buildAuto auto undefined--epsClasses
       in Automaton (symbSet Set.\\ Set.singleton "\\epsilon") stateSet init term deltaMap

epsClasses auto = undefined
