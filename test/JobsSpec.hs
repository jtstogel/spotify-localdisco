{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module JobsSpec (spec) where

import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar)
import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import qualified Jobs
import Test.Hspec
import qualified Types.Job as Job

data TestResult = TestResult
  { result :: !Text
  }
  deriving (Generic)

instance ToJSON TestResult

testJob :: Jobs.Job TestResult
testJob = do
  Jobs.yieldStatus "Job is running!"
  return $ TestResult {result = "done"}

runTestJob :: IO Job.Job
runTestJob = do
  db <- Jobs.newDB

  v <- newEmptyMVar :: IO (MVar ())
  _ <- Jobs.runJobFinally db "jobs/testJob" (putMVar v ()) testJob
  takeMVar v

  Jobs.getJob db "jobs/testJob"

spec :: Spec
spec = do
  describe "jobs" $ do
    it "should give final answer" $ do
      runTestJob
        `shouldReturn` Job.Job
          { Job.done = True,
            Job.result = Just $ toJSON $ TestResult {result = "done"},
            Job.error = Nothing,
            Job.metadata = Nothing
          }
