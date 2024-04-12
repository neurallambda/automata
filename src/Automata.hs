{-

Generate sentences in given grammars.

PDA Special Symbols

Left of relation:
ANY to match any {symbol, state, stack}

State:
INITIAL
ACCEPT
REJECT


----------
NOTES:

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

Pushdown Automaton (PDA): 7-tuple (Q, Σ, Γ, δ, q₀, Z₀, F), where:
- Q : states
- Σ : input symbols
- Γ : stack symbols (the stack alphabet)
- δ : Q × (Σ ∪ {ε}) × Γ → P(Q × Γ*) is the transition function
- q₀ ∈ Q
- Z₀ ∈ Γ is the initial stack symbol
- F ⊆ Q is the set of accepting/final states

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
{-# LANGUAGE DeriveAnyClass #-}

module Automata where

import Data.Sequence ( Seq, ViewL(..), (<|), Seq(..), (|>) )
import qualified Data.Sequence as Seq
import Data.Foldable ( toList, foldl' )
import qualified Data.Map as M
import Data.Kind (Type)
import Data.Map.Extra (lookupMatchAny, Any(..), MatchAny(..))
import Data.Maybe (mapMaybe)
import Data.Aeson
import Data.Aeson.Types (Parser)
import qualified Data.Vector as Vector
import qualified Data.Text as T
import GHC.Generics (Generic)

generateLs :: forall m a s. (Machine m a s, Ord (L m a s), MatchAny (L m a s))
  => [(L m a s, R m a s)] -- transition relation
  -> (S m a s -> Bool) -- halting function
  -> [a] -- input symbols
  -> S m a s -- initial state
  -> [[L m a s]] -- lazy list of valid prefixes
generateLs transitions hlt syms initState = bfs [(initState, Empty)]
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
          haltingAccs = [toList acc | hlt state]
        in
          haltingAccs ++ bfs newQueue

pdaString ::
  [(L PDA Symbol (State, Stack), R PDA Symbol (State, Stack))]
  -> (S PDA Symbol (State, Stack) -> Bool) -- halting states
  -> [Symbol] -- input symbols (to possibly stand in for Any)
  -> S PDA Symbol (State, Stack) -- initial state
  -> [T.Text]
pdaString transitions haltStates syms initState =
  map T.concat
    $ mapMaybe (mapM f) (generateLs transitions haltStates syms initState)
  where
    f (PDAL (A a) _ _) = Just a
    f (PDAL Any _ _) = Nothing


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

class Machine m a (s :: Type) where
  data L m a s -- ^ the Left side of a delta function/relation
  data R m a s -- ^ the Right side of a delta function/relation
  data S m a s -- ^ the State of the Machine
  -- | update the state (ex apply stack ops)
  action :: R m a s -> S m a s -> S m a s
  -- | build an input (ex add a peek at the top of a stack)
  mkL :: a -> S m a s -> L m a s

-- | Run a machine on an input symbol
runStep :: (Machine m a s, Ord (L m a s), Show (L m a s), MatchAny (L m a s))
  => M.Map (L m a s) (R m a s) -- transition table
  -> S m a s -- state
  -> a -- single input
  -> Maybe (R m a s, S m a s) -- (transition value, new state)
runStep table st input =
  case lookupMatchAny (mkL input st) table of
    Just transition -> Just (transition, action transition st)
    Nothing -> Nothing -- no transition found

-- | Run a machine on a list of input symbols
runMachine :: (Machine m a s
              , Ord (L m a s)
              , Show (L m a s)
              , MatchAny (L m a s)
              )
  => M.Map (L m a s) (R m a s) -- transition table
  -> S m a s -- initial state
  -> [a] -- input symbols
  -> Maybe (R m a s, S m a s)
runMachine table initState = foldl' f $ Just (error "empty input", initState)
  where
    f (Just (_, state)) = runStep table state
    f Nothing = const Nothing


--------------------------------------------------
-- * Push down automata

data PDA

data PDAOp stack =
  NullOp
  | Push !stack
  | Pop
  deriving (Eq, Ord, Show)

instance FromJSON stack => FromJSON (PDAOp stack) where
  parseJSON (String "pop") = return Pop
  parseJSON (String "nullop") = return NullOp
  parseJSON (Array arr) = do
    op <- parseJSON (arr Vector.! 0)
    case op of
      "push" -> do
        stack <- parseJSON (arr Vector.! 1)
        return (Push stack)
      _ -> fail $ "Invalid PDAOp operation: " ++ T.unpack op
  parseJSON invalid = fail $ "Invalid PDAOp JSON: " ++ show invalid

instance ToJSON stack => ToJSON (PDAOp stack) where
  toJSON NullOp = String "nullop"
  toJSON Pop = String "pop"
  toJSON (Push stack) = Array $ Vector.fromList [String "push", toJSON stack]


----------
--

instance Machine PDA a (state, stack) where
  data L PDA a (state, stack) = PDAL (Any a) (Any state) (Maybe (Any stack)) deriving (Show, Ord, Eq)
  data R PDA a (state, stack) = PDAR state (PDAOp stack) deriving (Show, Ord, Eq)
  data S PDA a (state, stack) = PDAS state (Data.Sequence.Seq stack) deriving (Show, Ord, Eq)

  action (PDAR newState NullOp) (PDAS _ stack) = PDAS newState stack
  action (PDAR newState (Push x)) (PDAS _ stack) = PDAS newState (x Data.Sequence.<| stack)
  action (PDAR newState Pop) (PDAS _ stack) = PDAS newState (Seq.drop 1 stack)

  mkL a (PDAS st sk) = PDAL (A a) (A st) (A <$> viewl sk)

instance (Eq state, Eq stack, Eq a) => MatchAny (L PDA a (state, stack)) where
  matchAny (PDAL x0 x1 x2) (PDAL y0 y1 y2) = matchAny (x0, x1, x2) (y0, y1, y2)


----------
-- * JSON

parseAt :: FromJSON a => Vector.Vector Value -> Int -> Parser a
parseAt arr i = parseJSON (arr Vector.! i)

instance (FromJSON a, FromJSON state, FromJSON stack) => FromJSON (L PDA a (state, stack)) where
  parseJSON = withArray "(L PDA a (state, stack))" $ \arr -> do
    PDAL
      <$> (arr `parseAt` 0) -- input
      <*> (arr `parseAt` 1) -- fromState
      <*> (arr `parseAt` 2) -- stackTop

instance (ToJSON a, ToJSON state, ToJSON stack) => ToJSON (L PDA a (state, stack)) where
  toJSON (PDAL input fromState stackTop) = Array $ Vector.fromList
    [ toJSON input
    , toJSON fromState
    , toJSON stackTop
    ]

instance (FromJSON state, FromJSON stack) => FromJSON (R PDA a (state, stack)) where
  parseJSON = withArray "(R PDA a (state, stack))" $ \arr -> do
    PDAR
      <$> (arr `parseAt` 0) -- toState
      <*> (arr `parseAt` 1) -- action

instance (ToJSON state, ToJSON stack) => ToJSON (R PDA a (state, stack)) where
  toJSON (PDAR toState action) = Array $ Vector.fromList
    [ toJSON toState
    , toJSON action
    ]

newtype Transition = Transition (L PDA Symbol (State, Stack), R PDA Symbol (State, Stack))
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- instance FromJSON Transition where
--   -- parseJSON = withArray "Transition" $ \arr -> do
--   --   l <- arr `parseAt` 0
--   --   r <- arr `parseAt` 1
--   --   return (Transition (l, r))

-- instance ToJSON Transition where
--   -- toJSON (Transition (PDAL inp state1 stack1, PDAR state2 stack2)) = Array $ Vector.fromList
--   --   [ toJSON inp
--   --   , toJSON state1
--   --   , toJSON stack1
--   --   , toJSON state2
--   --   , toJSON stack2
--   --   ]

parseAction :: Value -> Parser (PDAOp Stack)
parseAction (String "nullop") = return NullOp
parseAction (String "pop") = return Pop
parseAction v = pushParser v
  where
    pushParser :: Value -> Parser (PDAOp Stack)
    pushParser = withArray "Push" $ \arr -> do
      op <- arr `parseAt` 0
      case op of
        String "push" -> Push <$> arr `parseAt` 1
        _ -> fail "Invalid push action"

data MachineType =
  PDA
  | TM
  | DFA
  deriving (Show, Eq, Generic, ToJSON)

instance FromJSON MachineType where
  parseJSON = withText "MachineType" $ \t ->
    case t of
      "PDA" -> return PDA
      "pda" -> return PDA

      "TM" -> return TM
      "tm" -> return TM

      "DFA" -> return DFA
      "dfa" -> return DFA
      _ -> fail "Invalid machine value"

data MachineSpec = MachineSpec
  { machine :: !MachineType
  , symbols :: ![Symbol]
  , rules :: ![Transition]
  }
  deriving (Show, Eq, Generic, ToJSON)

instance FromJSON MachineSpec where
  parseJSON = withObject "MachineSpec" $ \obj -> do
    machine <- obj .: "machine"
    rules <- obj .: "rules"
    symbols <- obj .: "symbols" >>= parseSymbols
    transitions <- mapM parseJSON rules
    return (MachineSpec machine symbols transitions)
    where
      parseSymbols :: Value -> Parser [Symbol]
      parseSymbols = withArray "symbols" $ \arr ->
        mapM parseJSON (toList arr)

type State = T.Text
type Stack = T.Text
type Symbol = T.Text

untransition :: Functor f => f Transition -> f (L PDA Symbol (State, Stack), R PDA Symbol (State, Stack))
untransition xs = f <$> xs
  where f (Transition x) = x

initialState :: S PDA Symbol (State, Stack)
initialState = PDAS "INITIAL" Seq.empty

halt :: S PDA a (State, stack) -> Bool
halt (PDAS "REJECT" _) = True
halt (PDAS "ACCEPT" _) = True
halt _ = False
