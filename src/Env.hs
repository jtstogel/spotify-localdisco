module Env
  ( Env (..),
    load,
  )
where

import Data.Either ()
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T

data Env = Env
  { spotifyClientID :: Text,
    spotifyClientSecret :: Text
  }
  deriving (Show)

buildEnv :: (Monad m) => (String -> m Text) -> m Env
buildEnv get = do
  sClientID <- get "SPOTIFY_CLIENT_ID"
  sClientSecret <- get "SPOTIFY_CLIENT_SECRET"
  return $
    Env
      { spotifyClientID = sClientID,
        spotifyClientSecret = sClientSecret
      }

eitherFromMaybe :: l -> Maybe r -> Either l r
eitherFromMaybe = (. fmap Right) . fromMaybe . Left

second :: (b -> b') -> (a, b) -> (a, b')
second f (a, b) = (a, f b)

parseLine :: String -> Maybe (String, Text)
parseLine line = second (T.pack . tail) . flip List.splitAt line <$> List.elemIndex '=' line

parseEnv :: String -> Either String (Map.Map String Text)
parseEnv = fmap Map.fromList . eitherFromMaybe "failed to parse env lines" . mapM parseLine . List.lines

eitherIO :: Either String a -> IO a
eitherIO = either fail return

load :: String -> IO Env
load filename = do
  content <- readFile filename

  envMap <- eitherIO $ parseEnv content
  let get k = eitherFromMaybe ("failed to find env key " ++ k) $ Map.lookup k envMap

  eitherIO $ buildEnv get
