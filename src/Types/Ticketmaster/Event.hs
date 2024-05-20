{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Types.Ticketmaster.Event
  ( Event (..),
    EmbeddedEventDetails (..),
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)
import qualified Types.Ticketmaster.Attraction as Attraction

newtype EmbeddedEventDetails = EmbeddedEventDetails
  { attractions :: Maybe [Attraction.Attraction]
  }
  deriving (Generic, Show)

data Event = Event
  { name :: Text,
    id :: Text,
    _embedded :: EmbeddedEventDetails
  }
  deriving (Generic, Show)

instance FromJSON EmbeddedEventDetails

instance FromJSON Event

instance ToJSON EmbeddedEventDetails

instance ToJSON Event
