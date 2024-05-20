{-# LANGUAGE DeriveGeneric #-}

module Types.Job
  ( Job (..),
    JobError (..),
  )
where

import Data.Aeson (FromJSON, ToJSON, Value)
import Data.Text (Text)
import GHC.Generics (Generic)

data Job = Job
  -- Whether the job has finished (true iff one of error or result is populated).
  { done :: Bool,
    -- Error if the job failed.
    error :: Maybe JobError,
    -- The job's result!
    result :: Maybe Value,
    -- Details about the current state of the job.
    metadata :: Maybe Value
  }
  deriving (Generic, Show, Eq)

data JobError = JobError
  { code :: Int,
    message :: Text
  }
  deriving (Generic, Show, Eq)

instance FromJSON JobError

instance ToJSON JobError

instance FromJSON Job

instance ToJSON Job
