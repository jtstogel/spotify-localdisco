{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module JobsSpec (spec) where

import Data.Ratio
import Data.List (isPrefixOf)
import GeoHash
import Test.Hspec
import qualified Jobs
import qualified Types.Job as Job
import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import Control.Concurrent (threadDelay)

data TestResult = TestResult
  { result :: !Text
  }
  deriving (Generic)

instance ToJSON TestResult


testJob :: Jobs.Job TestResult
testJob = do
  Jobs.yieldStatus "Job is running!"
  return $ TestResult { result = "done" }


runTestJob :: IO Job.Job
runTestJob = do
  _ <- Jobs.runJob "jobs/testJob" testJob
  _ <- threadDelay 1000000
  Jobs.getJob "jobs/testJob"


spec :: Spec
spec = do
  describe "jobs" $ do
    it "should give final answer" $ do
      runTestJob `shouldReturn` Job.Job
        { Job.done = True
        , Job.result = Just $ toJSON $ TestResult { result = "done" }
        , Job.error = Nothing
        , Job.metadata = Nothing
        }
