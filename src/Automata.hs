{-

Automata class definition

--------------------------------------------------

Mealy Machine (FSM): 6-tuple (Q, Σ, Δ, δ, λ, q₀), where:
- Q : states
- Σ : input symbols
- Δ : output symbols
- δ : Q × Σ → Q is the state transition function
- λ : Q × Σ → Δ is the output function
- q₀ ∈ Q is the initial state

Moore Machine (FSM): 6-tuple (Q, Σ, Δ, δ, λ, q₀), where:
- Q : states
- Σ : input symbols
- Δ : output symbols
- δ : Q × Σ → Q is the state transition function
- λ : Q → Δ is the output function
- q₀ ∈ Q

Turing Machine (TM) with Queue: 8-tuple (Q, Σ, Γ, δ, q₀, q_acc, q_rej, □), where:
- Q : states
- Σ : input symbols
- Γ : queue symbols (the queue alphabet), where Σ ⊆ Γ and □ ∈ Γ \ Σ (□ is the blank symbol)
- δ : Q × Γ → Q × (Γ ∪ {ε}) × {EnqL, EnqR, DeqL, DeqR} is the transition function
- q₀ ∈ Q
- q_acc ∈ Q is the accepting state
- q_rej ∈ Q is the rejecting state
- □ is the blank symbol

Queue Automaton, Γ is head, Γ* is queue:
δ : Q × Γ → Q × Γ*

Multi-tape, with k tapes:
δ : Q × Γ_k → Q × Γ_k × { L , R }

-}

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

module Automata where

import Data.Sequence ( Seq, ViewL(..), Seq(..), (|>) )
import qualified Data.Sequence as Seq
import Data.Foldable ( toList, foldl' )
import Data.Kind (Type)
import Data.List.Extra (lookupMatchAny, MatchAny(..), Any (..))
import Data.Aeson ( Value, ToJSON, FromJSON(parseJSON) )
import Data.Aeson.Types (Parser)
import qualified Data.Vector as Vector
import Control.Arrow (second)
import qualified Data.PQueue.Min as Q

--------------------------------------------------
-- * Util

viewl :: Data.Sequence.Seq a -> Maybe a
viewl s = case Seq.viewl s of
  Data.Sequence.EmptyL -> Nothing
  (x Data.Sequence.:< _) -> Just x

viewr :: Data.Sequence.Seq a -> Maybe a
viewr s = case Seq.viewr s of
  Seq.EmptyR -> Nothing
  (_ Seq.:> x) -> Just x

splitAtR :: Int -> Data.Sequence.Seq a -> (Data.Sequence.Seq a, Data.Sequence.Seq a)
splitAtR i s = Seq.splitAt (length s - i) s

taker :: Int -> Data.Sequence.Seq a -> Data.Sequence.Seq a
taker i s = snd $ splitAtR i s

dropr :: Int -> Data.Sequence.Seq a -> Data.Sequence.Seq a
dropr i s = fst $ splitAtR i s


--------------------------------------------------
-- * Machine Definition

data Exit =
  Running
  | Error
  | Success
  deriving (Eq, Show)

-- | Generate valid strings (DFS version). Deprecated?
generateLsDFS :: forall m a s. (Machine m a s, Ord (L m a s), MatchAny (L m a s))
  => Int -- depth limit
  -> [(L m a s, R m a s)] -- transition relation
  -> (S m a s -> Exit) -- halting function
  -> [a] -- input symbols
  -> S m a s -- initial state
  -> [[L m a s]] -- lazy list of valid strings up to the specified depth
generateLsDFS maxDepth transitions hlt syms initState = dfs initState Empty 0
  where
    dfs :: S m a s -> Seq (L m a s) -> Int -> [[L m a s]]
    dfs state acc depth
      | depth > maxDepth = []
      | hlt state == Success = [toList acc]
      | hlt state == Error = []
      | otherwise = concatMap explore syms
      where
        explore a =
          let ls = filter (\(l, _) -> matchAny l (mkL a state)) transitions
          in concatMap (\(l, r) -> dfs (action r state) (acc |> l) (depth + 1)) ls

-- | Generate valid strings (BFS version). Deprecated?
generateLsBFS :: forall m a s. (Machine m a s, Ord (L m a s), MatchAny (L m a s))
  => [(L m a s, R m a s)] -- transition relation
  -> (S m a s -> Exit) -- halting function
  -> [a] -- input symbols
  -> S m a s -- initial state
  -> [[L m a s]] -- lazy list of valid prefixes
generateLsBFS transitions hlt syms initState = bfs [(initState, Empty)]
  where
    bfs :: [(S m a s, Seq (L m a s))] -> [[L m a s]]
    bfs [] = []
    bfs ((state, acc) : queue) = let
          -- find all valid transitions for each input symbol
          validTransitions = concatMap (\a ->
            let ls = filter (\(l, _) -> matchAny l (mkL a state)) transitions
            in map (\(l, r) -> (action r state, acc |> l)) ls
            ) syms
          -- add new states and their accumulators to the queue
          newQueue = queue ++ validTransitions
          -- check if the current state is a halting state
          haltingAccs = [toList acc | hlt state == Success]
        in
          haltingAccs ++ bfs newQueue

-- | Generate valid strings (Iterative Depth DFS).
--
-- If no strings have been found for a span of `maxDeepening`, stop generating
-- early (ie don't search ad infinitum)
generateLsIDDFS :: forall m a s. (Machine m a s, Ord (L m a s), MatchAny (L m a s))
  => Int -- maximum string length
  -> Int -- maximum deepening steps without finding new strings
  -> [(L m a s, R m a s)] -- transition relation
  -> (S m a s -> Exit) -- halting function
  -> [a] -- input symbols
  -> S m a s -- initial state
  -> [[L m a s]] -- lazy list of valid strings up to the specified depth
generateLsIDDFS maxLength maxDeepening transitions hlt syms initState = idfs [(initState, Empty, 0)] 0
  where
    idfs :: [(S m a s, Seq (L m a s), Int)] -> Int -> [[L m a s]]
    idfs queue deepeningSteps
      | deepeningSteps > maxDeepening = []
      | otherwise =
        let (haltingAccs, nonHaltingStates) = partitionHalting queue
            newQueue = concatMap exploreStates $ filter (\(_, _, len) -> len <= maxLength) nonHaltingStates
            validStrings = map (\(acc, _) -> toList acc) haltingAccs
            deepeningSteps' = if null validStrings then deepeningSteps + 1 else 0
        in validStrings ++ idfs newQueue deepeningSteps'

    partitionHalting :: [(S m a s, Seq (L m a s), Int)] -> ([(Seq (L m a s), Int)], [(S m a s, Seq (L m a s), Int)])
    partitionHalting = foldr f ([], [])
      where
        f (state, acc, len) (halting, nonHalting)
          | hlt state == Success = ((acc, len) : halting, nonHalting)
          | hlt state == Error = (halting, nonHalting)
          | otherwise = (halting, (state, acc, len) : nonHalting)

    exploreStates :: (S m a s, Seq (L m a s), Int) -> [(S m a s, Seq (L m a s), Int)]
    exploreStates (state, acc, len) = concatMap explore syms
      where
        explore a =
          let ls = filter (\(l, _) -> matchAny l (mkL a state)) transitions
          in map (\(l, r) -> (action r state, acc |> l, len + 1)) ls



data Scored a s = Scored !Float !a !s

instance Eq (Scored a s) where
  (Scored x _ _) == (Scored y _ _) = x == y

instance Ord (Scored a s) where
  (Scored x _ _) < (Scored y _ _) = x < y
  (Scored x _ _) <= (Scored y _ _) = x <= y
  (Scored x _ _) > (Scored y _ _) = x > y
  (Scored x _ _) >= (Scored y _ _) = x >= y

-- | Generate valid strings (Priority Queue).
generateLsPQ :: forall m a s. (Machine m a s, Ord (L m a s), MatchAny (L m a s))
  => Int -- maximum string length
  -> (Seq (L m a s) -> S m a s -> Float) -- scoring function
  -> [(L m a s, R m a s)] -- transition relation
  -> (S m a s -> Exit) -- halting function
  -> [a] -- input symbols
  -> S m a s -- initial state
  -> [[L m a s]] -- lazy list of valid strings
generateLsPQ maxLength score transitions hlt syms initState = pq initQueue
  where
    initQueue = Q.singleton (Scored (score Empty initState) Empty initState)

    pq :: Q.MinQueue (Scored (Seq (L m a s)) (S m a s)) -> [[L m a s]]
    pq queue = case Q.minView queue of
      Nothing -> []
      Just (Scored _ acc state, queue')
        | length acc > maxLength -> pq queue'
        | hlt state == Success -> toList acc : pq queue'
        | hlt state == Error -> pq queue'
        | otherwise ->
          let newStates = concatMap (exploreState acc state) syms
              newQueue = foldl' (flip Q.insert) queue' newStates
          in pq newQueue

    exploreState :: Seq (L m a s) -> S m a s -> a -> [Scored (Seq (L m a s)) (S m a s)]
    exploreState acc state sym =
      let ls = filter (\(l, _) -> matchAny l (mkL sym state)) transitions
      in map (\(l, r) ->
            let newState = action r state
                newAcc = acc |> l
            in Scored (score newAcc newState) newAcc newState
          ) ls

class
  (ToJSON (L m a s),
   FromJSON (L m a s),
   ToJSON (R m a s),
   FromJSON (R m a s)
  ) =>  Machine m a (s :: Type) where
  data L m a s -- ^ the Left side of a delta function/relation
  data R m a s -- ^ the Right side of a delta function/relation
  data S m a s -- ^ the State of the Machine
  -- | update the state (ex apply stack ops)
  action :: R m a s -> S m a s -> S m a s
  -- | build an input (ex add a peek at the top of a stack)
  mkL :: a -> S m a s -> L m a s

-- | Run a machine on an input symbol
runStep :: (Machine m a s, Eq (L m a s), Show (L m a s), MatchAny (L m a s), Eq (R m a s))
  => [(L m a s, R m a s)] -- transition table
  -> S m a s -- state
  -> a -- single input
  -> Maybe (R m a s, S m a s) -- (transition value, new state)
runStep table st input =
  case lookupMatchAny (mkL input st, Any) (second A <$> table) of
    Just (_, A transition) -> Just (transition, action transition st)
    _ -> Nothing -- no transition found

-- | Run a machine on a list of input symbols
runMachine :: (Machine m a s
              , Eq (L m a s)
              , Eq (R m a s)
              , Show (L m a s)
              , MatchAny (L m a s)
              )
  => [(L m a s, R m a s)] -- transition table
  -> S m a s -- initial state
  -> [a] -- input symbols
  -> Maybe (R m a s, S m a s)
runMachine table initState = foldl' f $ Just (error "empty input", initState)
  where
    f (Just (_, state)) = runStep table state
    f Nothing = const Nothing

parseAt :: FromJSON a => Vector.Vector Value -> Int -> Parser a
parseAt arr i = parseJSON (arr Vector.! i)
