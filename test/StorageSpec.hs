{-# LANGUAGE OverloadedStrings #-}

module StorageSpec (spec) where

import Control.Concurrent.MVar (MVar, newMVar, putMVar, takeMVar)
import Control.Monad (join)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import qualified Storage
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec (Spec, describe, it, shouldBe)

-- Helper function to create and use a test database
runTestDatabaseT :: Storage.DatabaseT IO a -> IO a
runTestDatabaseT action = join . withTestDBHandle $ flip Storage.runWithDB action

withTestDBHandle :: (Storage.DBHandle -> a) -> IO a
withTestDBHandle action = do
  withSystemTempDirectory "spotify-localdisco-test" $ \tmpDir -> do
    let tmpFile = T.pack tmpDir <> "/db.sqlite"
    pool <- Storage.createDatabaseHandle tmpFile 1
    return $ action pool

increment :: MVar Int -> IO Int
increment v = do
  x <- takeMVar v
  putMVar v (x + 1)
  return $ x + 1

spec :: Spec
spec = do
  describe "entities" $ do
    it "inserts and retrieves a user by UUID" $ do
      let user = Storage.DiscoUser {Storage.discoUserUuid = "uuid-1234", Storage.discoUserSpotifyId = "spotify-5678"}
      maybeUser <- liftIO . runTestDatabaseT $ Storage.upsertDiscoUser user >> Storage.getDiscoUser "uuid-1234"
      maybeUser `shouldBe` Just user

    it "inserts and retrieves a user by Spotify ID" $ do
      let user = Storage.DiscoUser {Storage.discoUserUuid = "uuid-1234", Storage.discoUserSpotifyId = "spotify-5678"}
      maybeUser <- liftIO . runTestDatabaseT $ Storage.upsertDiscoUser user >> Storage.getDiscoUserBySpotifyId "spotify-5678"
      maybeUser `shouldBe` Just user

  describe "runWithCache" $ do
    it "does not recompute" $ do
      results <- join $ withTestDBHandle $ \handle -> do

        a <- newMVar 0
        let incrementCached = Storage.runWithCache handle "key" 60 (increment a)
        _ <- incrementCached
        cachedResult <- incrementCached

        b <- newMVar 0
        _ <- increment b
        uncachedResult <- increment b

        return (cachedResult, uncachedResult)

      results `shouldBe` (1, 2)

    it "evicts results after expiry" $ do
      results <- join $ withTestDBHandle $ \handle -> do
        a <- newMVar 0
        -- Zero cache duration means it'll get evicted on the next lookup.
        -- We could properly stub out system time and use a real duration,
        -- but ¯\_(ツ)_/¯.
        let incrementCached = Storage.runWithCache handle "key" 0 (increment a)
        _ <- incrementCached
        incrementCached

      results `shouldBe` 2
