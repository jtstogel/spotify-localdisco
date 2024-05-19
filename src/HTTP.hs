{-# LANGUAGE FlexibleInstances #-}

module HTTP
  ( QueryParam(..)
  )
  where

import Data.ByteString (ByteString)
import Data.Text (Text, pack)
import Data.Text.Encoding (encodeUtf8)

class QueryParam a where
  queryParam :: a -> Maybe ByteString

instance QueryParam Text where
  queryParam = Just . encodeUtf8

instance QueryParam String where
  queryParam = queryParam . pack

instance QueryParam Int where
  queryParam = queryParam . show
