{-# LANGUAGE TupleSections #-}

module Lib
    ( isDFA
    , isNFA
    , isComplete
    , isMinimal
    , doComplete
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
isComplete auto = isDFA auto && noAloneSt && noAloneSymb
  where
    noAloneSt   = null $ states auto Set.\\ availableSt auto 
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
    null $ (Set.fromList . equiveClasses $ auto) Set.\\ (Set.map Set.singleton . states $ comAuto)


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

----------------------------------------------------------------------------------------------------
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

    bottom = "bottom"
    bottomSet = Set.singleton bottom

    transformStSetSymb :: (State, Set Symb) -> [(State, Symb)]
    transformStSetSymb (st, symbSet) = fmap (st,) . Set.toList $ symbSet

    addStSymb = Map.filter (not . Set.null) . Map.map ((Set.\\) (sigma auto)) . symbsByEverySt $ auto

    addDelta = concatMap (fmap (, bottomSet) . transformStSetSymb). Map.toList $ addStSymb

    bottomDelta = fmap (\x -> ((bottom, x), bottomSet)) . Set.toList . sigma $ auto

