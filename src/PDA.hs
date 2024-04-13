{-

Generate sentences in given grammars.

PDA Special Symbols

Left of relation:
ANY to match any {symbol, state, stack}

State:
INITIAL
ACCEPT
REJECT


Pushdown Automaton (PDA): 7-tuple (Q, Σ, Γ, δ, q₀, Z₀, F), where:
- Q : states
- Σ : input symbols
- Γ : stack symbols (the stack alphabet)
- δ : Q × (Σ ∪ {ε}) × Γ → P(Q × Γ*) is the transition function
- q₀ ∈ Q
- Z₀ ∈ Γ is the initial stack symbol
- F ⊆ Q is the set of accepting/final states

-}

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

module PDA where

import Automata (generateLsIDDFS, Machine(..), viewl)
import Data.Sequence ( Seq, (<|), Seq(..) )
import qualified Data.Sequence as Seq
import Data.Map.Extra (Any(..), MatchAny(..))
import Data.Maybe (mapMaybe)
import Data.Aeson
import Data.Aeson.Types (Parser)
import qualified Data.Vector as Vector
import qualified Data.Text as T

pdaString ::
  Int  -- max string length
  -> Int -- max steps to deepen before giving up
  -> [(L PDA Symbol (State, Stack), R PDA Symbol (State, Stack))]
  -> (S PDA Symbol (State, Stack) -> Bool) -- halting states
  -> [Symbol] -- input symbols (to possibly stand in for Any)
  -> S PDA Symbol (State, Stack) -- initial state
  -> [T.Text]
pdaString maxLen maxDeepening transitions haltStates syms initState =
  map T.concat
    $ mapMaybe (mapM f) (generateLsIDDFS maxLen maxDeepening transitions haltStates syms initState)
  where
    f (PDAL (A a) _ _) = Just a
    f (PDAL Any _ _) = Nothing

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

instance (ToJSON a
         , FromJSON a
         , ToJSON state
         , FromJSON state
         , ToJSON stack
         , FromJSON stack
         )
  => Machine PDA a (state, stack) where
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
  toJSON (PDAR toState act) = Array $ Vector.fromList [ toJSON toState, toJSON act]
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

type State = T.Text
type Stack = T.Text
type Symbol = T.Text

initialState :: S PDA Symbol (State, Stack)
initialState = PDAS "INITIAL" Seq.empty

halt :: S PDA a (State, stack) -> Bool
halt (PDAS "REJECT" _) = True
halt (PDAS "ACCEPT" _) = True
halt _ = False
