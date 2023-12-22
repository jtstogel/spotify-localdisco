module Env
    ( Env(..)
    , load
    ) where

import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Text as T

import Data.Either ()
import Data.Maybe (fromMaybe)
import Data.Text (Text(..))

data Env = Env
  { spotifyClientID :: Text
  , spotifyClientSecret :: Text
  } deriving (Show)

buildEnv :: Map.Map String String ->  Either String Env
buildEnv env = do
  let get = \k -> eitherFromMaybe ("Failed to find env key " ++ k) $ Map.lookup k env

  sClientID <- get "SPOTIFY_CLIENT_ID"
  sClientSecret <- get "SPOTIFY_CLIENT_SECRET"
  return $ Env
    { spotifyClientID = T.pack sClientID
    , spotifyClientSecret = T.pack sClientSecret
    }

eitherFromMaybe :: l -> Maybe r -> Either l r
eitherFromMaybe l = fromMaybe (Left l) . (fmap Right)

parseLine :: String -> Maybe (String, String)
parseLine line = do
  i <- List.elemIndex '=' line
  return (take i line, drop (i+1) line)

parseEnv :: String -> Either String Env
parseEnv text = do
  keyValues <- eitherFromMaybe "Failed to parse env lines" . mapM parseLine . List.lines $ text
  buildEnv $ Map.fromList keyValues

load :: String -> IO Env
load filename = do
    content <- readFile filename
    case parseEnv content of
        Right env -> return env
        Left msg -> error msg
