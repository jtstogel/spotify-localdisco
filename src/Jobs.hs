{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{- HLINT ignore "Use newtype instead of data" -}

module Jobs
  ( runJob,
    runJobFinally,
    getJob,
    yieldStatus,
    JobStatus (..),
    Job,
    DB,
    newDB,
  )
where

import Control.Concurrent (forkFinally)
import Control.Concurrent.STM.TVar (TVar, modifyTVar, newTVarIO, readTVar)
import qualified Control.Exception as Exception
import Control.Monad.Coroutine (Coroutine, pogoStickM)
import Control.Monad.Coroutine.SuspensionFunctors (Yield (..), yield)
import Control.Monad.STM (atomically)
import Data.Aeson (ToJSON, toJSON)
import qualified Data.Aeson as Aeson
import qualified Data.Map as M
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Errors (ErrStatus (..))
import GHC.Generics (Generic)
import Network.HTTP.Types.Status (status404, statusCode, statusMessage)
import qualified Types.Job as Job

newtype DB = DB (TVar (M.Map Text JobDetails))

newDB :: IO DB
newDB = DB <$> newTVarIO M.empty

data JobDetails = JobDetails
  { status :: Maybe JobStatus,
    err :: Maybe ErrStatus,
    result :: Maybe Aeson.Value
  }

defaultDetails :: JobDetails
defaultDetails =
  JobDetails
    { status = Nothing,
      err = Nothing,
      result = Nothing
    }

data JobStatus = JobStatus
  { message :: !Text
  }
  deriving (Generic, Show)

instance ToJSON JobStatus

type Job = Coroutine (Yield JobStatus) IO

setJobDetails :: DB -> Text -> JobDetails -> IO ()
setJobDetails (DB jobsVar) name = atomically . modifyTVar jobsVar . M.insert name

-- Runs a job in another thread and updates the shared job status at every yield.
runJobFinally :: (ToJSON r) => DB -> Text -> IO () -> Job r -> IO ()
runJobFinally db name finally coroutine = do
  let recordError e = setJobDetails db name $ defaultDetails {err = Just e}
  let recordResult r = setJobDetails db name $ defaultDetails {result = Just r}
  let recordStatus s = setJobDetails db name $ defaultDetails {status = Just s}
  let recordYieldedStatus (Yield s cont) = do _ <- recordStatus s; return cont

  _ <- recordStatus $ JobStatus {message = "Queued job..."}

  _ <-
    forkFinally
      ( do
          resultEither <- Exception.try $ pogoStickM recordYieldedStatus coroutine
          case resultEither of
            Left e -> recordError e
            Right r -> recordResult (toJSON r)
      )
      (const finally)

  return ()

runJob :: (ToJSON r) => DB -> Text -> Job r -> IO ()
runJob db name = runJobFinally db name (return ())

mkPublicErr :: ErrStatus -> Job.JobError
mkPublicErr (ErrStatus s msg) =
  Job.JobError
    { Job.code = statusCode s,
      Job.message = T.pack msg <> ": " <> decodeUtf8 (statusMessage s)
    }

getJob :: DB -> Text -> IO Job.Job
getJob (DB jobsVar) name = do
  job <- atomically . fmap (M.lookup name) . readTVar $ jobsVar

  case job of
    Nothing -> Exception.throw (ErrStatus status404 ("could not find job " <> T.unpack name))
    Just j ->
      return
        Job.Job
          { Job.done = isJust (err j) || isJust (result j),
            Job.error = mkPublicErr <$> err j,
            Job.result = result j,
            Job.metadata = toJSON <$> status j
          }

yieldStatus :: (Monad m) => Text -> Coroutine (Yield JobStatus) m ()
yieldStatus msg = yield $ JobStatus {message = msg}
