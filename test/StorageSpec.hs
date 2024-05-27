{-# LANGUAGE OverloadedStrings #-}

module StorageSpec (spec) where

import Control.Monad.IO.Class (liftIO)
import qualified Storage
import qualified Data.Text as T
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

-- Helper function to create and use a test database
withTestDB :: Storage.DatabaseT IO a -> IO a
withTestDB action = do
  withSystemTempDirectory "spotify-localdisco-test" $ \tmpDir -> do
    let tmpFile = (T.pack tmpDir) <> "/db.sqlite"
    pool <- Storage.createDatabaseHandle tmpFile 1
    Storage.runWithDB pool action

spec :: Spec
spec = do
  describe "storage" $ do
    it "inserts and retrieves a user by UUID" $ do
      let user = Storage.DiscoUser {Storage.discoUserUuid = "uuid-1234", Storage.discoUserSpotifyId = "spotify-5678"}
      maybeUser <- liftIO . withTestDB $ Storage.upsertDiscoUser user >> Storage.getDiscoUser "uuid-1234"
      maybeUser `shouldBe` Just user

    it "inserts and retrieves a user by Spotify ID" $ do
      let user = Storage.DiscoUser {Storage.discoUserUuid = "uuid-1234", Storage.discoUserSpotifyId = "spotify-5678"}
      maybeUser <- liftIO . withTestDB $ Storage.upsertDiscoUser user >> Storage.getDiscoUserBySpotifyId "spotify-5678"
      maybeUser `shouldBe` Just user
