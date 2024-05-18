{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Types.Ticketmaster.SearchEventsResponse
  ( SearchEventsResponse(..)
  , PageObject(..)
  , EmbeddedResponseDetails(..)
  )
  where

import Data.Maybe (fromMaybe)
import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import qualified Types.Ticketmaster.Attraction as Attraction
import qualified Types.Ticketmaster.Event as Event

data PageObject = PageObject
  { number :: Int
  , totalPages :: Int
  }
  deriving (Generic, Show)

data EmbeddedResponseDetails = EmbeddedResponseDetails
  { events :: Maybe [Event.Event]
  }
  deriving (Generic, Show)

data SearchEventsResponse = SearchEventsResponse
  { page :: PageObject
  , _embedded :: EmbeddedResponseDetails
  }
  deriving (Generic, Show)

instance FromJSON PageObject
instance FromJSON EmbeddedResponseDetails
instance FromJSON SearchEventsResponse

instance ToJSON PageObject
instance ToJSON EmbeddedResponseDetails
instance ToJSON SearchEventsResponse
