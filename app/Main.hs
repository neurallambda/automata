{-

USAGE:

cabal run automata -- --input rules/anbn.json --output data/anbn_progs.json --number 100 --max_length 100

# or

cabal build
dist/build/automata/automata --input rules/anbn.json --output anbn_progs.json --number 1000 --max_length 100

-}

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}

module Main where

import Options.Applicative

import qualified Data.ByteString.Char8 as B8

import Data.Aeson (FromJSON(..), ToJSON(..), encode, eitherDecodeStrict', withText, (.:))
import qualified Data.ByteString.Char8 as BC8
import GHC.Generics (Generic)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Automata as A
import qualified FSM
import qualified PDA
import qualified QA
import qualified Data.Aeson.Types as Aeson


data CLIOptions = CLIOptions
  { inputFile :: !FilePath
  , numGenerations :: !Int
  , outputFile :: !FilePath
  , maxStringLength :: !Int
  , maxDeepening :: !Int
  }

cliOptions :: ParserInfo CLIOptions
cliOptions = info (options <**> helper)
  ( fullDesc
    <> progDesc "Generate sentences from a given grammar"
    <> header "automata - a sentence generator for formal grammars"
  )
  where
    options = CLIOptions
      <$> strOption
        ( long "input"
        <> short 'i'
        <> metavar "FILE"
        <> help "Input file containing transition table")
      <*> option auto
        ( long "number"
        <> short 'n'
        <> metavar "INT"
        <> value 20
        <> showDefault
        <> help "Number of generations to produce")
      <*> strOption
        ( long "output"
        <> short 'o'
        <> metavar "FILE"
        <> help "Output file to save the generated data")
      <*> option auto
        ( long "max_length"
        <> short 'l'
        <> metavar "INT"
        <> value 20
        <> showDefault
        <> help "Max string length.")
      <*> option auto
        ( long "max_deepening"
        <> short 'd'
        <> metavar "INT"
        <> value 20
        <> showDefault
        <> help "Max number of depths to explore if no new strings have been yielded for this span.")

data MachineType =
  FSM -- Finite State Machine
  | PDA -- Pushdown Automaton
  | QA -- Queue Automaton
  deriving (Show, Eq, Generic, ToJSON)

instance FromJSON MachineType where
  parseJSON = withText "MachineType" $ \t ->
    case T.toLower t of
      "fsm" -> return FSM
      "pda" -> return PDA
      "qa" -> return QA
      _ -> fail "Invalid machine value"

-- First pass of parsing pulls out the machine type so we know how to parse the rest
newtype PartialMachineSpec = PartialMachineSpec
  { pMachine :: MachineType
  } deriving (Generic)

instance FromJSON PartialMachineSpec where
  parseJSON = Aeson.withObject "PartialMachineSpec" $ \v -> do
    machineType <- v .: "machine"
    return PartialMachineSpec
      { pMachine = machineType
      }

data MachineSpec m a s = MachineSpec
  { machine :: !MachineType
  , symbols :: ![Text]
  , rules :: ![(A.L m a s, A.R m a s)]
  } deriving Generic


instance FromJSON (MachineSpec FSM.FSM FSM.Symbol FSM.State) where
instance ToJSON (MachineSpec FSM.FSM FSM.Symbol FSM.State) where

instance FromJSON (MachineSpec PDA.PDA PDA.Symbol PDA.Frame) where
instance ToJSON (MachineSpec PDA.PDA PDA.Symbol PDA.Frame) where

instance FromJSON (MachineSpec QA.QA QA.Symbol QA.Frame) where
instance ToJSON (MachineSpec QA.QA QA.Symbol QA.Frame) where


data Output m a s = Output
  { spec :: !(MachineSpec m a s)
  , sentences :: ![Text]
  } deriving Generic

instance ToJSON (Output FSM.FSM FSM.Symbol FSM.State) where
instance ToJSON (Output PDA.PDA PDA.Symbol PDA.Frame) where
instance ToJSON (Output QA.QA QA.Symbol QA.Frame) where

main :: IO ()
main = do
  clio@CLIOptions{..} <- execParser cliOptions

  -- Read transition table from input file
  jsonInput <- B8.readFile inputFile
  case eitherDecodeStrict' @PartialMachineSpec jsonInput of
    Left err -> putStrLn $ "Error parsing `machine` key: " ++ err
    Right PartialMachineSpec{..} -> do
      case pMachine of
        FSM -> do
          case eitherDecodeStrict' jsonInput of
            Left err -> putStrLn $ "Error parsing FSM machine specification: " ++ err
            Right spec@MachineSpec{..} -> do
              strings <- FSM.fsmString maxStringLength maxDeepening numGenerations rules FSM.halt symbols FSM.initialState
              processOutput clio spec strings

        PDA -> do
          case eitherDecodeStrict' jsonInput of
            Left err -> putStrLn $ "Error parsing PDA machine specification: " ++ err
            Right spec@MachineSpec{..} -> do
              strings <- PDA.pdaString maxStringLength maxDeepening numGenerations rules PDA.halt symbols PDA.initialState
              processOutput clio spec strings

        QA -> case eitherDecodeStrict' jsonInput of
            Left err -> putStrLn $ "Error parsing QA machine specification: " ++ err
            Right spec@MachineSpec{..} -> do
              strings <- QA.qaString maxStringLength maxDeepening numGenerations rules QA.halt symbols QA.initialState
              processOutput clio spec strings

processOutput :: ToJSON (Output m a s) => CLIOptions -> MachineSpec m a s -> [Text] -> IO ()
processOutput CLIOptions{..} spec strings = do
  let out = Output spec strings

  -- Save the generated data
  if null strings
    then
      putStrLn "WARNING: No programs generated, perhaps you have an error in your transition rules?"
    else do
      BC8.writeFile outputFile $ BC8.toStrict (encode out)
      putStrLn "sample:"
      let hd = take 5 strings
          tl = drop (length strings - 5) strings
      mapM_ print hd
      putStrLn "..."
      mapM_ print tl
      putStrLn $ "generated " <> show (length strings) <> " total programs"
      putStrLn $ "saved to: " ++ outputFile
