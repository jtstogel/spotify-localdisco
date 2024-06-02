{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Types.Ticketmaster.SearchEventsResponse
  ( SearchEventsResponse (..),
    PageObject (..),
    EmbeddedResponseDetails (..),
  )
where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import qualified Types.Ticketmaster.Event as Event

data PageObject = PageObject
  { number :: Int,
    totalPages :: Int
  }
  deriving (Generic, Show)

newtype EmbeddedResponseDetails = EmbeddedResponseDetails
  { events :: Maybe [Event.Event]
  }
  deriving (Generic, Show)

data SearchEventsResponse = SearchEventsResponse
  { page :: PageObject,
    _embedded :: Maybe EmbeddedResponseDetails
  }
  deriving (Generic, Show)

instance FromJSON PageObject

instance FromJSON EmbeddedResponseDetails

instance FromJSON SearchEventsResponse

instance ToJSON PageObject

instance ToJSON EmbeddedResponseDetails

instance ToJSON SearchEventsResponse
