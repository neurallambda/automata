

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

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

qascore :: Seq (L QA Symbol State) -> S QA Symbol State -> Float
qascore xs state = fromIntegral $ Seq.length xs

qaString ::
  Int  -- max string length
  -> Int -- max steps to deepen before giving up
  -> Int -- number of strings to return
  -> [(L QA Symbol State, R QA Symbol State)]

  -> (S QA Symbol State -> Exit) -- halting states
  -> [Symbol] -- input symbols (to possibly stand in for Any)
  -> S QA Symbol State -- initial state
  -> IO [T.Text]
qaString maxLen maxDeepening nStrings transitions haltStates syms initState = do
  let f (QAL (A a) _) = Just a
      f (QAL Any _) = Nothing

  let randomFactor = 0.02  -- for randomizedID how many of possible branches to explore
  strs <- generateLsRandomizedID maxLen nStrings randomFactor transitions haltStates syms initState
  -- strs <- generateLsRandom maxLen nStrings transitions haltStates syms initState
  -- strs <- pure . take nStrings $ generateLsPQ maxLen qascore transitions haltStates syms initState
  -- strs <- pure . take nStrings $ generateLsIDDFS maxLen maxDeepening transitions haltStates syms initState
  -- strs <- pure . take nStrings $ generateLsDFS maxDeepening transitions haltStates syms initState

  pure . Set.toList . Set.fromList $ map T.concat $ mapMaybe (mapM f) strs

data QA

data QAOp a =
  EnqL a
  | EnqR a

  | DeqL
  | DeqR
  deriving (Eq, Ord, Show)

instance FromJSON a => FromJSON (QAOp a) where
  parseJSON (String "deql") = return DeqL
  parseJSON (String "deqr") = return DeqR
  parseJSON (Array arr) = do
    op <- parseJSON (arr Vector.! 0)
    case op of
      "enql" -> EnqL <$> parseJSON (arr Vector.! 1)
      "enqr" -> EnqR <$> parseJSON (arr Vector.! 1)
      _ -> fail $ "Invalid QAOp operation: " ++ T.unpack op
  parseJSON invalid = fail $ "Invalid QAOp JSON: " ++ show invalid

instance ToJSON a => ToJSON (QAOp a) where
  toJSON DeqL = String "deql"
  toJSON DeqR = String "deqr"
  toJSON (EnqL a) = Array $ Vector.fromList [String "enql", toJSON a]
  toJSON (EnqR a) = Array $ Vector.fromList [String "enqr", toJSON a]

instance (ToJSON a
  , FromJSON a
  , ToJSON state
  , FromJSON state
  )
  => Machine QA a state where
  data L QA a state = QAL (Any a) (Any state) deriving (Show, Ord, Eq)
  data R QA a state = QAR state (QAOp a) deriving (Show, Ord, Eq)
  data S QA a state = QAS state (Data.Sequence.Seq a) deriving (Show, Ord, Eq)

  action (QAR newState DeqL) (QAS _ q) = QAS newState (Seq.drop 1 q)
  action (QAR newState DeqR) (QAS _ q) = QAS newState (dropRight 1 q)

  action (QAR newState (EnqL x)) (QAS _ q) = QAS newState (x <| q)
  action (QAR newState (EnqR x)) (QAS _ q) = QAS newState (q |> x)

  mkL a (QAS st q) = QAL (A a) (A st)

instance (Eq state, Eq a) => MatchAny (L QA a state) where
  matchAny (QAL x0 x1) (QAL y0 y1) = matchAny (x0, x1) (y0, y1)

-- * JSON

parseAt :: FromJSON a => Vector.Vector Value -> Int -> Parser a
parseAt arr i = parseJSON (arr Vector.! i)

instance (FromJSON a, FromJSON state) => FromJSON (L QA a state) where
  parseJSON = withArray "(L QA a state)" $ \arr -> do
    QAL
    <$> (arr `parseAt` 0) -- input
    <*> (arr `parseAt` 1) -- fromState

instance (ToJSON a, ToJSON state) => ToJSON (L QA a state) where
  toJSON (QAL input fromState) = Array $ Vector.fromList
    [ toJSON input
    , toJSON fromState
    ]

instance (FromJSON state, FromJSON a) => FromJSON (R QA a state) where
  parseJSON = withArray "(R QA a state)" $ \arr -> do
    QAR
    <$> (arr `parseAt` 0) -- toState
    <*> (arr `parseAt` 1) -- action

instance (ToJSON state, ToJSON a) => ToJSON (R QA a state) where
  toJSON (QAR toState act) = Array $ Vector.fromList [ toJSON toState, toJSON act]

type State = T.Text

type Symbol = T.Text

initialState :: S QA Symbol State

initialState = QAS "INITIAL" Seq.empty

halt :: S QA a Text -> Exit
halt (QAS "REJECT" _) = Error
halt (QAS "ACCEPT" _) = Success

halt _ = Running
