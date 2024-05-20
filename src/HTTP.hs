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
import Data.Text (Text, pack, unpack)
import Text.Read (readMaybe)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
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

maybeHead :: [a] -> Maybe a
maybeHead []  = Nothing
maybeHead (x:_) = Just x

retryAfter :: Response a -> Maybe Int
retryAfter response = do
  header <- maybeHead $ getResponseHeader "Retry-After" response
  seconds <- readMaybe . unpack $ decodeUtf8 header

  -- Cap at 30 seconds. We don't have all day.
  return $ if seconds > 30
    then 30
    else seconds

getWithAttempts :: (FromJSON r, Show r) => Int -> Request -> IO r
getWithAttempts retries request = do
  response <- httpJSONEither (traceShowId request)
  let code = getResponseStatusCode (traceShowId response)

  let shouldRetry = code `elem` [429, 500]
  let ok = code `elem` [200]

  let result | shouldRetry = do
                 let delaySeconds = fromMaybe 5 $ retryAfter response
                 threadDelay $ delaySeconds * 1000000
                 if retries <= 0
                   then throwErr status500 "quota exceeded!"
                   else getWithAttempts (retries - 1) request
             | ok          = eitherStatusIO status500 . mapLeft show . getResponseBody $ response
             | otherwise   = throwErr (getResponseStatus response) "failed to get"
  result

get :: (FromJSON r, Show r) => Request -> IO r
get request = getWithAttempts 3 request
