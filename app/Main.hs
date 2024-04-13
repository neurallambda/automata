{-

USAGE:

cabal run automata -- --input rules/anbn.json --output data/anbn_progs.json --number 100 --max_string_length 100

# or

cabal build
dist/build/automata/automata --input rules/anbn.json --output anbn_progs.json --number 1000 --max_string_length 100

-}

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}

module Main where

import Options.Applicative

import qualified Data.ByteString.Char8 as B8

import Data.Aeson (FromJSON(..), ToJSON(..), encode, eitherDecodeStrict', withText)
import qualified Data.ByteString.Char8 as BC8
import GHC.Generics (Generic)
import Data.Text (Text)
import PDA

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
  FSM
  | PDA
  | TM
  deriving (Show, Eq, Generic, ToJSON)

instance FromJSON MachineType where
  parseJSON = withText "MachineType" $ \t ->
    case t of
      "FSM" -> return FSM
      "fsm" -> return FSM

      "PDA" -> return PDA
      "pda" -> return PDA

      "TM" -> return TM
      "tm" -> return TM

      _ -> fail "Invalid machine value"


data MachineSpec m a s = MachineSpec
  { machine :: !MachineType
  , symbols :: ![Text]
  , rules :: ![(L m a s, R m a s)]
  } deriving Generic

instance FromJSON (MachineSpec PDA.PDA Text (Text, Text)) where
instance ToJSON (MachineSpec PDA.PDA Text (Text, Text)) where


data Output m a s = Output
  { spec :: !(MachineSpec m a s)
  , sentences :: ![Text]
  } deriving Generic

instance ToJSON (Output PDA.PDA Text (Text, Text)) where


main :: IO ()
main = do
  CLIOptions{..} <- execParser cliOptions

  -- Read transition table from input file
  jsonInput <- B8.readFile inputFile
  case (eitherDecodeStrict' @(MachineSpec PDA Text (Text, Text))) jsonInput of
    Left err -> putStrLn $ "Error parsing machine specification: " ++ err
    Right machineSpec -> do
      let spec@MachineSpec{..} = machineSpec
          strings = take numGenerations $ PDA.pdaString maxStringLength maxDeepening rules PDA.halt symbols PDA.initialState
          out = Output spec strings

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
