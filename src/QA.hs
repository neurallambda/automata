{-

Queue Automaton(ish)

Instead of a normal FIFO queue, this automata has access to both sides of the
queue (IE, it's a Sequence).

Also, a Queue Automaton usually treats its queue as its input, but this version
works like the PDA where it separates Input, Frame, and, Queue

-}

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module QA where

import Automata (generateLsIDDFS, Machine(..), viewl, viewr, generateLsDFS, Exit, generateLsPQ, generateLsRandom, generateLsRandomizedID)
import Data.Sequence ( Seq, (|>), Seq(..), (<|) )
import qualified Data.Sequence as Seq
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

dropRight :: Int -> Seq a -> Seq a
dropRight n s = Seq.take (Seq.length s - n) s

qascore :: Seq (L QA Symbol Frame) -> S QA Symbol Frame -> Float
qascore xs frame = fromIntegral $ Seq.length xs

qaString ::
  Int  -- max string length
  -> Int -- max steps to deepen before giving up
  -> Int -- number of strings to return
  -> [(L QA Symbol Frame, R QA Symbol Frame)]

  -> (S QA Symbol Frame -> Exit) -- halting states
  -> [Symbol] -- input symbols (to possibly stand in for Any)
  -> S QA Symbol Frame -- initial state
  -> IO [T.Text]
qaString maxLen maxDeepening nStrings transitions haltStates syms initState = do
  let f (QAL (A a) _ _) = Just a
      f (QAL Any _ _) = Nothing

  -- let randomFactor = 0.2  -- for randomizedID how many of possible branches to explore
  -- strs <- generateLsRandomizedID maxLen nStrings randomFactor transitions haltStates syms initState
  -- strs <- generateLsRandom maxLen nStrings transitions haltStates syms initState
  -- strs <- pure . take nStrings $ generateLsPQ maxLen qascore transitions haltStates syms initState
  strs <- pure . take nStrings $ generateLsIDDFS maxLen maxDeepening transitions haltStates syms initState
  -- strs <- pure . take nStrings $ generateLsDFS maxDeepening transitions haltStates syms initState
  pure . Set.toList . Set.fromList $ map T.concat $ mapMaybe (mapM f) strs

data QA

data QAOp a =
  EnqL !a
  | EnqR !a
  | DeqL
  | DeqR
  | Nullop
  deriving (Eq, Ord, Show)

instance FromJSON a => FromJSON (QAOp a) where
  parseJSON (String "NULLOP") = return Nullop
  parseJSON (String "DEQL") = return DeqL
  parseJSON (String "DEQR") = return DeqR
  parseJSON (Array arr) = do
    op <- parseJSON (arr Vector.! 0)
    case op of
      "ENQL" -> EnqL <$> parseJSON (arr Vector.! 1)
      "ENQR" -> EnqR <$> parseJSON (arr Vector.! 1)
      _ -> fail $ "Invalid QAOp operation: " ++ T.unpack op
  parseJSON invalid = fail $ "Invalid QAOp JSON: " ++ show invalid

instance ToJSON a => ToJSON (QAOp a) where
  toJSON DeqL = String "DEQL"
  toJSON Nullop = String "NULLOP"
  toJSON DeqR = String "DEQR"
  toJSON (EnqL a) = Array $ Vector.fromList [String "ENQL", toJSON a]
  toJSON (EnqR a) = Array $ Vector.fromList [String "ENQR", toJSON a]


type Symbol = T.Text
type State  = T.Text
type Queue = T.Text
type Frame = (State, Queue)

instance (ToJSON a
  , FromJSON a
  , ToJSON state
  , FromJSON state
  , ToJSON queue
  , FromJSON queue
  )
  => Machine QA a (state, queue) where
  data L QA a (state, queue) = QAL (Any a) (Any state) (Maybe (Any queue)) deriving (Show, Ord, Eq)
  data R QA a (state, queue) = QAR state (QAOp queue) deriving (Show, Ord, Eq)
  data S QA a (state, queue) = QAS state (Data.Sequence.Seq queue) deriving (Show, Ord, Eq)

  action (QAR newState Nullop) (QAS _ q) = QAS newState q
  action (QAR newState DeqL) (QAS _ q) = QAS newState (Seq.drop 1 q)
  action (QAR newState DeqR) (QAS _ q) = QAS newState (dropRight 1 q)
  action (QAR newState (EnqL x)) (QAS _ q) = QAS newState (x <| q)
  action (QAR newState (EnqR x)) (QAS _ q) = QAS newState (q |> x)

  mkL a (QAS st q) = QAL (A a) (A st) (A <$> viewl q)

instance (Eq state, Eq queue, Eq a) => MatchAny (L QA a (state, queue)) where
  matchAny (QAL x0 x1 x2) (QAL y0 y1 y2) = matchAny (x0, x1, x2) (y0, y1, y2)

-- * JSON

parseAt :: FromJSON a => Vector.Vector Value -> Int -> Parser a
parseAt arr i = parseJSON (arr Vector.! i)

instance (FromJSON a, FromJSON state, FromJSON queue) => FromJSON (L QA a (state, queue)) where
  parseJSON = withArray "(L QA a frame)" $ \arr -> do
    QAL
    <$> (arr `parseAt` 0) -- input
    <*> (arr `parseAt` 1) -- fromState
    <*> (arr `parseAt` 2) -- queueHead

instance (ToJSON a, ToJSON state, ToJSON queue) => ToJSON (L QA a (state, queue)) where
  toJSON (QAL input fromState queueHead) = Array $ Vector.fromList
    [ toJSON input
    , toJSON fromState
    , toJSON queueHead
    ]

instance (FromJSON state, FromJSON queue, FromJSON a) => FromJSON (R QA a (state, queue)) where
  parseJSON = withArray "(R QA a frame)" $ \arr -> do
    QAR
    <$> (arr `parseAt` 0) -- toState
    <*> (arr `parseAt` 1) -- action

instance (ToJSON state, ToJSON queue, ToJSON a) => ToJSON (R QA a (state, queue)) where
  toJSON (QAR toState act) = Array $ Vector.fromList [ toJSON toState, toJSON act]


initialState :: S QA Symbol Frame
initialState = QAS "INITIAL" Seq.empty

halt :: S QA a Frame -> Exit
halt (QAS "REJECT" _) = Error
halt (QAS "ACCEPT" _) = Success
halt _ = Running
