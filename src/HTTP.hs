{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module HTTP
  ( QueryParam(..)
  , get
  )
  where

import Control.Concurrent (threadDelay)
import Control.Monad (when)
import Data.Aeson
import Data.ByteString (ByteString)
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack)
import Data.Text.Encoding (encodeUtf8)
import Debug.Trace (traceShowId)
import Errors
import Network.HTTP.Simple
import Network.HTTP.Types.Status (status500)

class QueryParam a where
  queryParam :: a -> Maybe ByteString

instance QueryParam Text where
  queryParam = Just . encodeUtf8

instance QueryParam String where
  queryParam = queryParam . pack

instance QueryParam Int where
  queryParam = queryParam . show

instance (QueryParam a) => QueryParam (Maybe a) where
  queryParam = (>>= queryParam)


getWithAttempts :: (FromJSON r, Show r) => Int -> Request -> IO r
getWithAttempts 0 _ = throwErr status500 ("retried maximum number of times")
getWithAttempts attempts request = do
  response <- httpJSONEither (traceShowId request)
  let code = getResponseStatusCode response

  let shouldRetry = code `elem` [429, 500]
  let ok = code `elem` [200]

  let result | shouldRetry = do
                   threadDelay 10000000
                   getWithAttempts (attempts - 1) request
             | ok          = eitherStatusIO status500 . mapLeft show . getResponseBody $ response
             | otherwise   = throwErr (getResponseStatus response) "failed to get"
  result

get :: (FromJSON r, Show r) => Request -> IO r
get request = getWithAttempts 5 request
