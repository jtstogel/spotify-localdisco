module Env
  ( Env (..),
    load,
    loadFromFile,
  )
where

import Data.Either ()
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import Errors (eitherFromMaybe, eitherIO, maybeIO)
import System.Environment (lookupEnv)

data Env = Env
  { spotifyClientID :: Text,
    spotifyClientSecret :: Text,
    ticketmasterConsumerKey :: Text
  }
  deriving (Show)

buildEnv :: (Monad m) => (String -> m Text) -> m Env
buildEnv get = do
  sClientID <- get "SPOTIFY_CLIENT_ID"
  sClientSecret <- get "SPOTIFY_CLIENT_SECRET"
  tConsumerKey <- get "TICKETMASTER_CONSUMER_KEY"
  return $
    Env
      { spotifyClientID = sClientID,
        spotifyClientSecret = sClientSecret,
        ticketmasterConsumerKey = tConsumerKey
      }

second :: (b -> b') -> (a, b) -> (a, b')
second f (a, b) = (a, f b)

parseLine :: String -> Maybe (String, Text)
parseLine line = second (T.pack . tail) . flip List.splitAt line <$> List.elemIndex '=' line

parseEnv :: String -> Either String (Map.Map String Text)
parseEnv = fmap Map.fromList . eitherFromMaybe "failed to parse env lines" . mapM parseLine . List.lines

loadFromFile :: String -> IO Env
loadFromFile filename = do
  content <- readFile filename

  envMap <- eitherIO $ parseEnv content
  let get k = eitherFromMaybe ("failed to find env key " ++ k) $ Map.lookup k envMap

  eitherIO $ buildEnv get

getFromEnv :: String -> IO Text
getFromEnv variable = do
  maybeValue <- lookupEnv variable
  value <- maybeIO ("failed to lookup environment variable for " <> variable) maybeValue
  return $ T.pack value

load :: IO Env
load = buildEnv getFromEnv
