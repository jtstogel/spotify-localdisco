{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import Data.List (find)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Data.Time.Clock (getCurrentTime, addUTCTime, nominalDay)
import Env (Env (spotifyClientID))
import Errors (eitherStatusIO, eitherIO, maybeIO, ErrStatus(..))
import Network.HTTP.Types.Status (Status(statusMessage), status404)
import Network.Wai                       (Middleware)
import Network.Wai.Middleware.Cors       (CorsResourcePolicy(..), cors, simpleCorsResourcePolicy)
import qualified App
import qualified CreatePlaylist
import qualified Data.ByteString.Lazy as B
import qualified Data.Text as T
import qualified Data.Text.Lazy as LazyText
import qualified Env
import qualified Locations
import qualified Spotify
import qualified Ticketmaster
import qualified Types.GetSpotifyClientIdResponse as GetSpotifyClientIdResponse
import qualified Types.Spotify.ListTopItemsRequest as ListTopItemsRequest
import qualified Types.Ticketmaster.ListArtistsResponse as ListArtistsResponse
import qualified Types.Ticketmaster.SearchEventsRequest as SearchEventsRequest
import qualified Web.Scotty as S
import Text.Read (readMaybe)
import Web.Scotty (ActionM)

main :: IO ()
main = do
  env <- Env.load ".env"
  postalCodeLookup <- loadPostalCodeLookup "postal-codes.json"

  let appState =
        App.AppState
          { App.spotifyClientID = Env.spotifyClientID env
          , App.spotifyClientSecret = Env.spotifyClientSecret env
          , App.ticketmasterConsumerKey = Env.ticketmasterConsumerKey env
          , App.ticketmasterConsumerSecret = Env.ticketmasterConsumerSecret env
          , App.postalCodeLookup = postalCodeLookup
          }

  routes appState

loadPostalCodeLookup :: String -> IO Locations.PostalCodeLookup
loadPostalCodeLookup filename = do
  content <- B.readFile filename
  entries <- eitherIO $ (eitherDecode content :: Either String [Locations.PostalCodeLocation])
  return $ Locations.buildPostalCodeLookup entries

handleException :: ErrStatus -> ActionM ()
handleException (ErrStatus s msg) = do
    S.status s
    S.text . LazyText.fromStrict $ (T.pack msg) <> ": " <> (decodeUtf8 $ statusMessage s) 

getSpotifyClientId :: App.AppState -> ActionM ()
getSpotifyClientId appState = do
  S.json $
    GetSpotifyClientIdResponse.GetSpotifyClientIdResponse
      { GetSpotifyClientIdResponse.clientId = App.spotifyClientID appState
      }

queryParamMaybe :: LazyText.Text -> ActionM (Maybe LazyText.Text)
queryParamMaybe name = do
  params <- S.queryParams
  return . fmap snd . find ((==name) . fst) $ params

getEvents :: App.AppState -> ActionM ()
getEvents appState = do
  postalCode <- S.queryParam "postalCode" :: ActionM Text
  geoHash <- liftIO $ eitherStatusIO status404 $ Locations.lookupGeoHash (App.postalCodeLookup appState) postalCode
  radiusMiles <- S.queryParam "radiusMiles" :: ActionM Int
  days <- S.queryParam "days" :: ActionM Int
  startTime <- liftIO getCurrentTime
  let endTime = addUTCTime ((fromIntegral days) * nominalDay) startTime

  pageToken <- queryParamMaybe "pageToken"
  let pageNumberMaybe = readMaybe . LazyText.unpack . fromMaybe "0" $ pageToken :: Maybe Int
  pageNumber <- liftIO . maybeIO "failed to parse pageToken" $ pageNumberMaybe

  events <- liftIO $ CreatePlaylist.searchEvents (App.ticketmasterConsumerKey appState)
    SearchEventsRequest.SearchEventsRequest
      { SearchEventsRequest.geoHash = T.pack . take 9 $ geoHash
      , SearchEventsRequest.radiusMiles = radiusMiles
      , SearchEventsRequest.classificationName = ["music"]
      , SearchEventsRequest.startTime = startTime
      , SearchEventsRequest.endTime = endTime
      , SearchEventsRequest.pageSize = 200
      , SearchEventsRequest.pageNumber = pageNumber
      }
    50000

  S.json $ ListArtistsResponse.fromEvents events

getGeoHash :: App.AppState -> ActionM ()
getGeoHash appState = do
  postalCode <- S.queryParam "postalCode" :: ActionM Text
  geoHash <- liftIO $ eitherStatusIO status404 $ Locations.lookupGeoHash (App.postalCodeLookup appState) postalCode
  S.text $ LazyText.pack geoHash

getPlaceName :: App.AppState -> ActionM ()
getPlaceName appState = do
  postalCode <- S.queryParam "postalCode" :: ActionM Text
  placeName <- liftIO $ eitherStatusIO status404 $ Locations.lookupPlaceName (App.postalCodeLookup appState) postalCode
  S.text $ LazyText.fromStrict placeName

getTopArtists :: App.AppState -> ActionM ()
getTopArtists appState = do
  auth <- S.queryParam "spotifyAccessToken" :: ActionM  Text
  limit <- S.queryParam "limit" :: ActionM Int
  artists <- liftIO $ CreatePlaylist.getTopArtists auth limit
  S.json $ artists

getTopTracks :: App.AppState -> ActionM ()
getTopTracks appState = do
  auth <- S.queryParam "spotifyAccessToken" :: ActionM  Text
  limit <- S.queryParam "limit" :: ActionM Int
  tracks <- liftIO $ CreatePlaylist.getTopTracks auth limit
  S.json $ tracks

allowCors :: Middleware
allowCors = cors (const $ Just appCorsResourcePolicy)

appCorsResourcePolicy :: CorsResourcePolicy
appCorsResourcePolicy =
    simpleCorsResourcePolicy
        { corsMethods = ["OPTIONS", "GET", "PUT", "POST"]
        , corsRequestHeaders = ["content-type"]
        }

routes :: App.AppState -> IO ()
routes appState = S.scotty 8080 $ do
  S.defaultHandler (S.Handler handleException)
  S.middleware allowCors

  S.get "/spotify/clientId" $ getSpotifyClientId appState
  S.get "/spotify/topArtists" $ getTopArtists appState
  S.get "/spotify/topTracks" $ getTopTracks appState
  S.get "/geohash" $ getGeoHash appState
  S.get "/placeName" $ getPlaceName appState
  S.get "/localArtists" $ getEvents appState
