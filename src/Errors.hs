module Errors where

import Control.Exception (Exception, throwIO)
import Data.Either (Either(Left, Right))
import Data.Maybe (fromMaybe)
import Data.Typeable (Typeable)
import Network.HTTP.Types.Status (Status, status500)

data ErrStatus = ErrStatus Status String
   deriving (Show, Typeable)

instance Exception ErrStatus

throwErr :: Status -> String -> IO a
throwErr s msg = throwIO $ ErrStatus s msg

eitherFromMaybe :: l -> Maybe r -> Either l r
eitherFromMaybe = (. fmap Right) . fromMaybe . Left

mapLeft :: (a -> b) -> Either a c -> Either b c
mapLeft f (Left v) = Left $ f v
mapLeft _ (Right v) = Right v

eitherStatusIO :: Status -> Either String b -> IO b
eitherStatusIO code = either throwIO return . (mapLeft (ErrStatus code))

eitherIO :: Either String b -> IO b
eitherIO = eitherStatusIO status500

maybeIO :: String -> Maybe a -> IO a
maybeIO nothingMessage = eitherIO . eitherFromMaybe nothingMessage
