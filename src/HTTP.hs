{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}

module HTTP
  ( QueryParam (..),
    get,
  )
where

import Control.Concurrent (threadDelay)
import Data.Aeson (FromJSON)
import Data.ByteString (ByteString)
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack, unpack)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Debug.Trace (traceShowId)
import Errors (eitherStatusIO, mapLeft, throwErr)
import Network.HTTP.Simple
  ( Request,
    Response,
    getResponseBody,
    getResponseHeader,
    getResponseStatus,
    getResponseStatusCode,
    httpJSONEither,
  )
import Network.HTTP.Types.Status (status500)
import Text.Read (readMaybe)

class QueryParam a where
  queryParam :: a -> Maybe ByteString

instance QueryParam Text where
  queryParam :: Text -> Maybe ByteString
  queryParam = Just . encodeUtf8

instance QueryParam String where
  queryParam :: String -> Maybe ByteString
  queryParam = queryParam . pack

instance QueryParam Int where
  queryParam :: Int -> Maybe ByteString
  queryParam = queryParam . show

instance (QueryParam a) => QueryParam (Maybe a) where
  queryParam :: Maybe a -> Maybe ByteString
  queryParam = (>>= queryParam)

maybeHead :: [a] -> Maybe a
maybeHead [] = Nothing
maybeHead (x : _) = Just x

retryAfter :: Response a -> Maybe Int
retryAfter response = do
  header <- maybeHead $ getResponseHeader "Retry-After" response
  seconds <- readMaybe . unpack $ decodeUtf8 header

  -- Cap at 30 seconds. We don't have all day.
  return $ min seconds 30

getWithRetries :: (FromJSON r, Show r) => Int -> Request -> IO r
getWithRetries retries request = do
  response <- httpJSONEither (traceShowId request)
  let code = getResponseStatusCode (traceShowId response)

  let shouldRetry = code `elem` [429, 500]
  let ok = code == 200

  let result
        | shouldRetry && retries == 0 = throwErr status500 "quota exceeded!"
        | shouldRetry && retries > 0 = do
            let delaySeconds = fromMaybe 5 $ retryAfter response
            threadDelay $ delaySeconds * 1000000
            getWithRetries (retries - 1) request
        | ok = eitherStatusIO status500 . mapLeft show . getResponseBody $ response
        | otherwise = throwErr (getResponseStatus response) "failed to get"
  result

get :: (FromJSON r, Show r) => Request -> IO r
get = getWithRetries 3
