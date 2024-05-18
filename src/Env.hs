module Env
  ( Env (..),
    load,
  )
where

import Data.Either ()
import Data.Text (Text)
import Errors (eitherIO, eitherFromMaybe)
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Text as T

data Env = Env
  { spotifyClientID :: Text
  , spotifyClientSecret :: Text
  , ticketmasterConsumerKey :: Text
  , ticketmasterConsumerSecret :: Text
  }
  deriving (Show)

buildEnv :: (Monad m) => (String -> m Text) -> m Env
buildEnv get = do
  sClientID <- get "SPOTIFY_CLIENT_ID"
  sClientSecret <- get "SPOTIFY_CLIENT_SECRET"
  tConsumerKey <- get "TICKETMASTER_CONSUMER_KEY"
  tConsumerSecret <- get "TICKETMASTER_CONSUMER_SECRET"
  return $
    Env
      { spotifyClientID = sClientID
      , spotifyClientSecret = sClientSecret
      , ticketmasterConsumerKey = tConsumerKey
      , ticketmasterConsumerSecret = tConsumerSecret
      }

second :: (b -> b') -> (a, b) -> (a, b')
second f (a, b) = (a, f b)

parseLine :: String -> Maybe (String, Text)
parseLine line = second (T.pack . tail) . flip List.splitAt line <$> List.elemIndex '=' line

parseEnv :: String -> Either String (Map.Map String Text)
parseEnv = fmap Map.fromList . eitherFromMaybe "failed to parse env lines" . mapM parseLine . List.lines

load :: String -> IO Env
load filename = do
  content <- readFile filename

  envMap <- eitherIO $ parseEnv content
  let get k = eitherFromMaybe ("failed to find env key " ++ k) $ Map.lookup k envMap

  eitherIO $ buildEnv get
