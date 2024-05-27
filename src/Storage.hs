{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Storage
  ( DiscoUser (..),
    SpotifyUserAuth (..),
    DBHandle,
    DatabaseT (..),
    runWithDB,
    createDatabaseHandle,
    getDiscoUser,
    upsertDiscoUser,
    getDiscoUserBySpotifyId,
    getSpotifyUserAuth,
    upsertSpotifyUserAuth,
    getSpotifyUserAuthByUser,
    deleteSpotifyUserAuthByUser
  )
where

import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Logger (NoLoggingT (runNoLoggingT))
import Control.Monad.Trans.Class (MonadTrans (..))
import Control.Monad.Trans.Reader (ReaderT)
import Data.Foldable (forM_)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Database.Persist.Sqlite (ConnectionPool, Entity (..), SqlBackend, createSqlitePool, delete, getBy, insert_, replace, runMigration, runSqlPool)
import Database.Persist.TH (mkMigrate, mkPersist, persistLowerCase, share, sqlSettings)

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
DiscoUser
    uuid Text
    UniqueDiscoUser uuid
    spotifyId Text
    UniqueSpotifyId spotifyId
    deriving Eq
    deriving Show

SpotifyUserAuth
    spotifyUserId Text
    UniqueSpotifyUserAuth spotifyUserId
    accessToken Text
    refreshToken Text
    expireTime UTCTime
    refreshing Bool
|]

newtype DatabaseT m a = DatabaseT {runDatabaseT :: ReaderT SqlBackend m a}

instance (Functor m) => Functor (DatabaseT m) where
  fmap :: (a -> b) -> DatabaseT m a -> DatabaseT m b
  fmap f (DatabaseT action) = DatabaseT (fmap f action)

instance (Applicative m) => Applicative (DatabaseT m) where
  pure :: a -> DatabaseT m a
  pure = DatabaseT . pure

  (<*>) :: DatabaseT m (a -> b) -> DatabaseT m a -> DatabaseT m b
  (DatabaseT f) <*> (DatabaseT a) = DatabaseT (f <*> a)

instance (Monad m) => Monad (DatabaseT m) where
  (>>=) :: DatabaseT m a -> (a -> DatabaseT m b) -> DatabaseT m b
  (DatabaseT action) >>= f = DatabaseT (action >>= runDatabaseT . f)

instance (MonadIO m) => MonadIO (DatabaseT m) where
  liftIO :: IO a -> DatabaseT m a
  liftIO = DatabaseT . liftIO

instance MonadTrans DatabaseT where
  lift :: (Monad m) => m a -> DatabaseT m a
  lift = DatabaseT . lift

type DBHandle = ConnectionPool

-- Runs the DatabaseT monad transactionally
runWithDB :: (MonadUnliftIO m) => DBHandle -> DatabaseT m a -> m a
runWithDB pool action = runSqlPool (runDatabaseT action) pool

-- Opens a DB connection pool given the SQLite file name
createDatabaseHandle :: Text -> Int -> IO DBHandle
createDatabaseHandle sqliteFile poolSize = runNoLoggingT $ do
  pool <- createSqlitePool sqliteFile poolSize
  runSqlPool (runMigration migrateAll) pool
  return pool

-- Gets a DiscoUser by their UUID
getDiscoUser :: (MonadUnliftIO m) => Text -> DatabaseT m (Maybe DiscoUser)
getDiscoUser uuid = DatabaseT $ do
  user <- getBy (UniqueDiscoUser uuid)
  return $ entityVal <$> user

-- Inserts or updates a DiscoUser by their UUID
upsertDiscoUser :: (MonadUnliftIO m) => DiscoUser -> DatabaseT m ()
upsertDiscoUser user = DatabaseT $ do
  let uuid = discoUserUuid user
  existingUser <- getBy (UniqueDiscoUser uuid)
  case existingUser of
    Just entity -> replace (entityKey entity) user
    Nothing -> insert_ user

-- Gets a DiscoUser by their Spotify ID
getDiscoUserBySpotifyId :: (MonadUnliftIO m) => Text -> DatabaseT m (Maybe DiscoUser)
getDiscoUserBySpotifyId spotifyId = DatabaseT $ do
  user <- getBy (UniqueSpotifyId spotifyId)
  return $ entityVal <$> user

getSpotifyUserAuthByUserId :: (MonadUnliftIO m) => Text -> DatabaseT m (Maybe SpotifyUserAuth)
getSpotifyUserAuthByUserId userId = do
  maybeUser <- getDiscoUser userId
  case discoUserSpotifyId <$> maybeUser of
    Just spotifyId -> getSpotifyUserAuth spotifyId
    Nothing -> return Nothing

getSpotifyUserAuthByUser :: (MonadUnliftIO m) => DiscoUser -> DatabaseT m (Maybe SpotifyUserAuth)
getSpotifyUserAuthByUser user = getSpotifyUserAuthByUserId $ discoUserUuid user

getSpotifyUserAuth :: (MonadUnliftIO m) => Text -> DatabaseT m (Maybe SpotifyUserAuth)
getSpotifyUserAuth spotifyUserId = DatabaseT $ do
  auth <- getBy (UniqueSpotifyUserAuth spotifyUserId)
  return $ entityVal <$> auth

upsertSpotifyUserAuth :: (MonadUnliftIO m) => SpotifyUserAuth -> DatabaseT m ()
upsertSpotifyUserAuth auth = DatabaseT $ do
  existingAuth <- getBy (UniqueSpotifyUserAuth $ spotifyUserAuthSpotifyUserId auth)
  case existingAuth of
    Just entity -> replace (entityKey entity) auth
    Nothing -> insert_ auth

deleteSpotifyUserAuthByUser :: (MonadUnliftIO m) => DiscoUser -> DatabaseT m ()
deleteSpotifyUserAuthByUser user = DatabaseT $ do
  maybeAuth <- getBy (UniqueSpotifyUserAuth . discoUserSpotifyId $ user)
  forM_ maybeAuth (delete . entityKey)
