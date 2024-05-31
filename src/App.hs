{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module App
  ( AppState (..),
    App,
    AppT,
    memoize,
    runWithDB,
    runAppT,
  )
where

import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (ReaderT, asks, lift, runReaderT)
import Data.Aeson (FromJSON, ToJSON, encode)
import Data.Text (Text, pack)
import Data.Time (NominalDiffTime)
import qualified Jobs
import qualified Locations
import qualified Storage
import Data.Text.Encoding (decodeUtf8)
import Data.ByteString (toStrict)

data AppState = AppState
  { spotifyClientID :: !Text,
    spotifyClientSecret :: !Text,
    ticketmasterConsumerKey :: !Text,
    ticketmasterConsumerSecret :: !Text,
    postalCodeLookup :: !Locations.PostalCodeLookup,
    jobsDB :: !Jobs.DB,
    dbHandle :: !Storage.DBHandle,
    port :: Int
  }

type AppT m = ReaderT AppState m

type App = AppT IO

runWithDB :: (MonadUnliftIO m) => Storage.DatabaseT m a -> AppT m a
runWithDB action = do
  handle <- asks dbHandle
  lift $ Storage.runWithDB handle action

mkCacheKey :: (ToJSON a) => a -> Text
mkCacheKey = decodeUtf8 . toStrict . encode

memoize :: (ToJSON r, ToJSON a, FromJSON a, MonadUnliftIO m) => App.AppState -> Text -> NominalDiffTime -> (r -> m a) -> r -> m a
memoize appState namespace duration f r = Storage.runWithCache (dbHandle appState) (namespace <> pack "/" <> mkCacheKey r) duration (f r)

runAppT :: AppState -> AppT m a -> m a
runAppT state app = runReaderT app state
