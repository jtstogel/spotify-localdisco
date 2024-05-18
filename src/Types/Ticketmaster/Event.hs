{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Types.Ticketmaster.Event
  ( Event(..)
  , EmbeddedEventDetails(..)
  )
  where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import qualified Types.Ticketmaster.Attraction as Attraction

data EmbeddedEventDetails = EmbeddedEventDetails
  { attractions :: Maybe [Attraction.Attraction]
  }
  deriving (Generic, Show)

data Event = Event
  { name :: Text
  , id :: Text
  , _embedded :: EmbeddedEventDetails
  }
  deriving (Generic, Show)

instance FromJSON EmbeddedEventDetails
instance FromJSON Event

instance ToJSON EmbeddedEventDetails
instance ToJSON Event
