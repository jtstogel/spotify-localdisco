{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module App
  ( AppState (..),
    App,
    AppT,
    runWithDB,
    runAppT,
  )
where

import Control.Monad.Reader (ReaderT, runReaderT, ask, lift)
import Data.Text (Text)
import qualified Jobs
import qualified Locations
import qualified Storage
import Control.Monad.IO.Unlift (MonadUnliftIO)

data AppState = AppState
  { spotifyClientID :: !Text,
    spotifyClientSecret :: !Text,
    ticketmasterConsumerKey :: !Text,
    ticketmasterConsumerSecret :: !Text,
    postalCodeLookup :: !Locations.PostalCodeLookup,
    jobsDB :: !Jobs.DB,
    dbHandle :: !Storage.DBHandle
  }

type AppT m = ReaderT AppState m
type App = AppT IO

runWithDB :: (MonadUnliftIO m) => Storage.DatabaseT m a -> AppT m a
runWithDB action = do
  handle <- dbHandle <$> ask
  lift $ Storage.runWithDB handle action

runAppT :: (MonadUnliftIO m) => AppState -> AppT m a -> m a 
runAppT state app = (runReaderT app) state
