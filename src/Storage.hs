{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE InstanceSigs #-}

module Storage
    ( DiscoUser(..),
      DBHandle,
      DatabaseT(..),
      runWithDB,
      createDatabaseHandle,
      getDiscoUser,
      upsertDiscoUser,
      getDiscoUserBySpotifyId
    )
    where

import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.Trans.Class (MonadTrans(..))
import Database.Persist.Sqlite (runSqlPool, createSqlitePool, runMigration, SqlBackend, ConnectionPool, insert_, replace, getBy, Entity (..))
import Database.Persist.TH (mkPersist, mkMigrate, persistLowerCase, share, sqlSettings)
import Data.Text (Text)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Logger (NoLoggingT(runNoLoggingT))
import Data.Time.Clock (UTCTime)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
DiscoUser
    uuid String
    UniqueDiscoUser uuid
    spotifyId String
    UniqueSpotifyId spotifyId
    deriving Eq
    deriving Show

SpotifyUserAuth
    spotifyId String
    UniqueSpotifyUserAuth spotifyId
    accessToken String
    refreshToken String
    expireTime UTCTime
|]

newtype DatabaseT m a = DatabaseT { runDatabaseT :: ReaderT SqlBackend m a }

instance Functor m => Functor (DatabaseT m) where
    fmap :: (a -> b) -> DatabaseT m a -> DatabaseT m b
    fmap f (DatabaseT action) = DatabaseT (fmap f action)

instance Applicative m => Applicative (DatabaseT m) where
    pure :: a -> DatabaseT m a
    pure = DatabaseT . pure

    (<*>) :: DatabaseT m (a -> b) -> DatabaseT m a -> DatabaseT m b
    (DatabaseT f) <*> (DatabaseT a) = DatabaseT (f <*> a)

instance Monad m => Monad (DatabaseT m) where
    (>>=) :: DatabaseT m a -> (a -> DatabaseT m b) -> DatabaseT m b
    (DatabaseT action) >>= f = DatabaseT (action >>= runDatabaseT . f)

instance MonadIO m => MonadIO (DatabaseT m) where
    liftIO :: IO a -> DatabaseT m a
    liftIO = DatabaseT . liftIO

instance MonadTrans DatabaseT where
    lift :: Monad m => m a -> DatabaseT m a
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
getDiscoUser :: (MonadUnliftIO m) => String -> DatabaseT m (Maybe DiscoUser)
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
getDiscoUserBySpotifyId :: (MonadUnliftIO m) => String -> DatabaseT m (Maybe DiscoUser)
getDiscoUserBySpotifyId spotifyId = DatabaseT $ do
    user <- getBy (UniqueSpotifyId spotifyId)
    return $ entityVal <$> user
