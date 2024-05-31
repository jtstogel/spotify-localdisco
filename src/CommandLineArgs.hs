{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{- HLINT ignore "Use newtype instead of data" -}

module CommandLineArgs
  ( Args (..),
    parse,
  )
where

import Data.Text (Text)
import Options.Applicative
  ( Parser,
    ParserInfo,
    auto,
    execParser,
    help,
    helper,
    info,
    long,
    metavar,
    option,
    progDesc,
    showDefault,
    strOption,
    value,
    (<**>),
  )

parse :: IO Args
parse = execParser opts

opts :: ParserInfo Args
opts =
  info
    (args <**> helper)
    (progDesc "Run the Local Disco web server")

data Args = Args
  { port :: Int,
    postalCodesPath :: Text,
    databasePath :: Text
  }

args :: Parser Args
args =
  Args
    <$> option
      auto
      ( long "port"
          <> help "Port to run the web server on."
          <> showDefault
          <> value 8080
          <> metavar "INT"
      )
    <*> strOption
      ( long "postal_codes_path"
          <> help "Path to JSON file containing postal code information."
          <> showDefault
          <> value "postal-codes.json"
          <> metavar "FILE_PATH"
      )
    <*> strOption
      ( long "database_path"
          <> help "Path to sqlite database."
          <> showDefault
          <> value "data/db.sqlite"
          <> metavar "FILE_PATH"
      )
