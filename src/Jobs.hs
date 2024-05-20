{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Jobs
  ( runJob
  , getJob
  , yieldStatus
  , JobStatus(..)
  , Job(..)
  )
  where

import System.IO.Unsafe (unsafePerformIO)
import Control.Concurrent (forkIO)
import Control.Concurrent.STM.TVar (newTVarIO, readTVar, modifyTVar, TVar)
import Control.Monad.Coroutine (pogoStickM, Coroutine)
import Control.Monad.Coroutine.SuspensionFunctors (Yield(..), yield)
import Control.Monad.Error.Class (catchError)
import Control.Monad.STM (STM, atomically)
import Data.Aeson (ToJSON, toJSON)
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Errors (ErrStatus(..))
import GHC.Generics
import Network.HTTP.Types.Status (status404, status500, statusCode, statusMessage)
import qualified Control.Exception as Exception
import qualified Data.Aeson as Aeson
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Types.Job as Job


data JobDetails = JobDetails
  { status :: Maybe JobStatus
  , err  :: Maybe ErrStatus
  , result :: Maybe Aeson.Value
  }

defaultDetails = JobDetails
  { status = Nothing
  , err = Nothing
  , result = Nothing
  }

data JobStatus = JobStatus
  { message :: Text
  }
  deriving (Generic, Show)

instance ToJSON JobStatus

type Job r = Coroutine (Yield JobStatus) IO r

-- Store jobs in a global variable because I'm LAZY.
jobsVar :: TVar (M.Map Text JobDetails)
jobsVar = unsafePerformIO $ newTVarIO M.empty

setJobDetails :: Text -> JobDetails -> IO ()
setJobDetails name details = atomically . modifyTVar jobsVar $ M.insert name details

-- Runs a job in another thread and updates the shared job status at every yield.
runJob :: (ToJSON r) => Text -> Job r -> IO ()
runJob name coroutine = do
  let recordError error   = setJobDetails name $ defaultDetails { err = Just error }
  let recordResult result = setJobDetails name $ defaultDetails { result = Just result }
  let recordStatus status = setJobDetails name $ defaultDetails { status = Just status }
  let recordYieldedStatus (Yield status cont) = do { _ <- recordStatus status; return cont }

  _ <- recordStatus $ JobStatus { message = "Queued job..." }

  _ <- forkIO $ do
    resultEither <- Exception.try $ pogoStickM recordYieldedStatus coroutine
    case resultEither of
      Left e -> case e of 
        ErrStatus _ _ -> recordError e
        _ -> recordError $ ErrStatus status500 (show e)
      Right r -> recordResult (toJSON r)

  return ()

mkPublicErr :: ErrStatus -> Job.JobError
mkPublicErr (ErrStatus s msg) = Job.JobError
  { Job.code = statusCode s
  , Job.message = T.pack msg <> ": " <> decodeUtf8 (statusMessage s)
  }

getJob :: Text -> IO Job.Job
getJob name = do
  job <- atomically . fmap (M.lookup name) . readTVar $ jobsVar

  case job of
    Nothing -> Exception.throw (ErrStatus status404 ("could not find job " <> (T.unpack name)))
    Just j -> return Job.Job
      { Job.done = isJust (err j) || isJust (result j)
      , Job.error = mkPublicErr <$> err j
      , Job.result = result j
      , Job.metadata = toJSON <$> status j
      }

yieldStatus :: Monad m => Text -> Coroutine (Yield JobStatus) m ()
yieldStatus msg = yield $ JobStatus { message = msg }
