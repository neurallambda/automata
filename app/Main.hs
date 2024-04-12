{-

USAGE:

cabal run automata -- --input rules/anbn.json --output anbn_progs.json --number 1000

# or

cabal build
dist/build/automata/automata --input rules/anbn.json --output anbn_progs.json --number 1000

-}

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}

module Main where

import Options.Applicative

import qualified Data.ByteString.Char8 as B8

import qualified Automata as A
import Data.Aeson (ToJSON, encode, eitherDecodeStrict')
import qualified Data.ByteString.Char8 as BC8
import GHC.Generics (Generic)
import Data.Text (Text)

data CLIOptions = CLIOptions
  { inputFile :: !FilePath
  , numGenerations :: !Int
  , outputFile :: !FilePath
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

data Output = Output
  { spec :: !A.MachineSpec
  , sentences :: ![Text]
  } deriving (Show, Generic, ToJSON)

main :: IO ()
main = do
  CLIOptions{..} <- execParser cliOptions

  -- Read transition table from input file
  jsonInput <- B8.readFile inputFile
  case eitherDecodeStrict' jsonInput of
    Left err -> putStrLn $ "Error parsing machine specification: " ++ err
    Right machineSpec -> do
      let spec@A.MachineSpec{..} = machineSpec
          strings = A.pdaString (A.untransition rules) A.halt symbols A.initialState
          out = Output spec (take numGenerations strings)

      -- Save the generated data to the output file
      BC8.writeFile outputFile $ BC8.toStrict (encode out)
      putStrLn $ "Generated data saved to: " ++ outputFile
      if null strings
        then
          putStrLn "WARNING: No programs generated, perhaps you have an error in your transition rules?"
        else do
          putStrLn "examples:"
          mapM_ print (take 10 strings)
          putStrLn "done."
