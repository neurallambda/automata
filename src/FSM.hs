{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

module FSM where

import Automata (generateLsIDDFS, Machine(..), generateLsDFS, Exit, generateLsPQ, generateLsRandom, generateLsRandomizedID)
import Data.List.Extra (Any(..), MatchAny(..))
import Data.Maybe (mapMaybe)
import Data.Aeson
    ( Value(Array, String),
      ToJSON(toJSON),
      FromJSON(parseJSON),
      withArray )
import Data.Aeson.Types (Parser)
import qualified Data.Vector as Vector
import qualified Data.Text as T
import Automata (Exit(..))
import qualified Data.Set as Set
import Data.Text (Text)

fsmscore :: [L FSM Symbol State] -> S FSM Symbol State -> Float
fsmscore xs state = fromIntegral $ length xs

fsmString ::
  Int  -- max string length
  -> Int -- max steps to deepen before giving up
  -> Int -- number of strings to return
  -> [(L FSM Symbol State, R FSM Symbol State)]
  -> (S FSM Symbol State -> Exit) -- halting states
  -> [Symbol] -- input symbols (to possibly stand in for Any)
  -> S FSM Symbol State -- initial state
  -> IO [T.Text]
fsmString maxLen maxDeepening nStrings transitions haltStates syms initState = do
  let f (FSML (A a) _) = Just a
      f (FSML Any _) = Nothing
  let randomFactor = 0.02  -- for randomizedID how many of possible branches to explore
  strs <- generateLsRandomizedID maxLen nStrings randomFactor transitions haltStates syms initState
  -- strs <- generateLsRandom maxLen nStrings transitions haltStates syms initState
  -- strs <- pure . take nStrings $ generateLsPQ maxLen fsmscore transitions haltStates syms initState
  -- strs <- pure . take nStrings $ generateLsIDDFS maxLen maxDeepening transitions haltStates syms initState
  -- strs <- pure . take nStrings $ generateLsDFS maxDeepening transitions haltStates syms initState
  pure . Set.toList . Set.fromList $ map T.concat $ mapMaybe (mapM f) strs

data FSM

instance (ToJSON a
         , FromJSON a
         , ToJSON state
         , FromJSON state
         )
  => Machine FSM a state where
  data L FSM a state = FSML (Any a) (Any state) deriving (Show, Ord, Eq)
  data R FSM a state = FSMR state deriving (Show, Ord, Eq)
  data S FSM a state = FSMS state deriving (Show, Ord, Eq)

  action (FSMR newState) (FSMS _) = FSMS newState

  mkL a (FSMS st) = FSML (A a) (A st)

instance (Eq state, Eq a) => MatchAny (L FSM a state) where
  matchAny (FSML x0 x1) (FSML y0 y1) = matchAny (x0, x1) (y0, y1)


----------
-- * JSON

parseAt :: FromJSON a => Vector.Vector Value -> Int -> Parser a
parseAt arr i = parseJSON (arr Vector.! i)

instance (FromJSON a, FromJSON state) => FromJSON (L FSM a state) where
  parseJSON = withArray "(L FSM a state)" $ \arr -> do
    FSML
      <$> (arr `parseAt` 0) -- input
      <*> (arr `parseAt` 1) -- fromState

instance (ToJSON a, ToJSON state) => ToJSON (L FSM a state) where
  toJSON (FSML input fromState) = Array $ Vector.fromList
    [ toJSON input
    , toJSON fromState
    ]

instance (FromJSON state) => FromJSON (R FSM a state) where
  parseJSON = withArray "(R FSM a state)" $ \arr -> do
    FSMR
      <$> (arr `parseAt` 0) -- toState

instance (ToJSON state) => ToJSON (R FSM a state) where
  toJSON (FSMR toState) = Array $ Vector.fromList [ toJSON toState ]

type State = T.Text
type Symbol = T.Text

initialState :: S FSM Symbol State
initialState = FSMS "INITIAL"

halt :: S FSM a Text -> Exit
halt (FSMS "REJECT") = Error
halt (FSMS "ACCEPT") = Success
halt _ = Running
