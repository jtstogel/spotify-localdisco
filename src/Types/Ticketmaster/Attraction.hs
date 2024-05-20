{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Types.Ticketmaster.Attraction
  ( Attraction (..),
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

data Attraction = Attraction
  { name :: Text,
    id :: Text
  }
  deriving (Generic, Show)

instance FromJSON Attraction

instance ToJSON Attraction
