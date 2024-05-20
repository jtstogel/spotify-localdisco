{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module JobsSpec (spec) where

import Control.Concurrent.MVar (MVar, newEmptyMVar, takeMVar, putMVar)
import Test.Hspec
import qualified Jobs
import qualified Types.Job as Job
import Data.Aeson
import Data.Text (Text)
import GHC.Generics

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
  v <- newEmptyMVar :: IO (MVar ())

  db <- Jobs.newDB


  _ <- Jobs.runJobFinally db "jobs/testJob" (putMVar v ()) testJob

  takeMVar v
  Jobs.getJob db "jobs/testJob"


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
