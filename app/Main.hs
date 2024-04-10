{-

USAGE:
cabal run automata -- -i rules/anbn.json -g 20
# or
cabal build
dist/build/automata/automata --input anbn.json --generations 20
-}

{-# LANGUAGE RecordWildCards #-}

module Main where

import Options.Applicative

import qualified Data.Sequence as Seq
import qualified Data.ByteString.Char8 as B8

import qualified Automata as A

data CLIOptions = CLIOptions
  { inputFile :: !FilePath
  , numGenerations :: !Int
  }

cliOptions :: Parser CLIOptions
cliOptions = CLIOptions
  <$> strOption
    ( long "input"
    <> short 'i'
    <> metavar "FILE"
    <> help "Input file containing transition table"
    )
  <*> option auto
    ( long "generations"
    <> short 'g'
    <> metavar "INT"
    <> value 20
    <> showDefault
    <> help "Number of generations to produce"
    )

main :: IO ()
main = do
  CLIOptions{..} <- execParser $ info (cliOptions <**> helper)
    ( fullDesc
    <> progDesc "Generate sentences from a given grammar"
    <> header "grammar-generator - a sentence generator for formal grammars"
    )

  -- Read transition table from input file
  jsonInput <- B8.readFile inputFile
  case A.parseMachineSpec jsonInput of
    Left err -> putStrLn $ "Error parsing machine specification: " ++ err
    Right machineSpec -> do
      let A.MachineSpec{..} = machineSpec
          initialState = A.PDAS A.Q0 Seq.empty
          strings = A.pdaString (A.untransition rules) A.halt symbols initialState
      mapM_ print (take numGenerations strings)
