{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Database.Schema
  ( migrateAll,
    User (..),
    SpotifyUserAuth (..),
  )
where

import Control.Monad.IO.Class (liftIO)
import Data.ByteString (ByteString)
import Data.Time (UTCTime)
import Database.Persist
  ( Entity,
    PersistQueryWrite (deleteWhere),
    PersistStoreRead (get),
    PersistStoreWrite (delete, insert),
    SelectOpt (LimitTo),
    selectList,
    (==.),
  )
import Database.Persist.Sqlite
  ( BackendKey (SqlBackendKey),
    runMigration,
    runSqlite,
  )
import Database.Persist.TH

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|

User
    uuid ByteString
    spotify_user_id String

SpotifyUserAuth
    user_id UserId
    access_token String
    refresh_token String
    expire_time UTCTime
    scope [String]
|]

