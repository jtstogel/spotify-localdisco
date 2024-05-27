{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified App
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ask)
import qualified CreatePlaylist
import Data.Aeson (KeyValue ((.=)), eitherDecode, object)
import qualified Data.ByteString.Lazy as B
import Data.List (find)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Text.Lazy as LazyText
import Data.Time.Clock (addUTCTime, getCurrentTime, nominalDay)
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUIDV4
import Env (Env (spotifyClientID))
import qualified Env
import Errors (ErrStatus (..), eitherIO, eitherStatusIO, maybeIO)
import qualified Jobs
import qualified Locations
import Network.HTTP.Types.Status (Status (statusMessage), status404)
import Network.Wai (Middleware)
import Network.Wai.Middleware.Cors (CorsResourcePolicy (..), cors, simpleCorsResourcePolicy)
import qualified Storage
import Text.Read (readMaybe)
import qualified Types.CreatePlaylistJobRequest as CreatePlaylistJobRequest
import qualified Types.GetSpotifyClientIdResponse as GetSpotifyClientIdResponse
import qualified Types.Ticketmaster.ListArtistsResponse as ListArtistsResponse
import qualified Types.Ticketmaster.SearchEventsRequest as SearchEventsRequest
import Web.Scotty (ActionM)
import qualified Web.Scotty as S

main :: IO ()
main = do
  env <- Env.load
  postalCodeLookup <- loadPostalCodeLookup "postal-codes.json"
  jobsDB <- Jobs.newDB
  dbHandle <- Storage.createDatabaseHandle "data/db.sqlite" 10

  let appState =
        App.AppState
          { App.spotifyClientID = Env.spotifyClientID env,
            App.spotifyClientSecret = Env.spotifyClientSecret env,
            App.ticketmasterConsumerKey = Env.ticketmasterConsumerKey env,
            App.ticketmasterConsumerSecret = Env.ticketmasterConsumerSecret env,
            App.postalCodeLookup = postalCodeLookup,
            App.jobsDB = jobsDB,
            App.dbHandle = dbHandle
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
  S.text . LazyText.fromStrict $ T.pack msg <> ": " <> decodeUtf8 (statusMessage s)

getSpotifyClientId :: App.AppT ActionM ()
getSpotifyClientId = do
  lift $ S.addHeader "Cache-Control" "no-store"
  clientId <- App.spotifyClientID <$> ask
  lift . S.json $
    GetSpotifyClientIdResponse.GetSpotifyClientIdResponse
      { GetSpotifyClientIdResponse.clientId = clientId
      }

queryParamMaybe :: LazyText.Text -> ActionM (Maybe LazyText.Text)
queryParamMaybe name = do
  params <- S.queryParams
  return . fmap snd . find ((== name) . fst) $ params

getEvents :: App.AppT ActionM ()
getEvents = do
  postalCode <- lift $ (S.queryParam "postalCode" :: ActionM Text)
  postalCodeLookup <- App.postalCodeLookup <$> ask
  ticketmasterConsumerKey <- App.ticketmasterConsumerKey <$> ask
  geoHash <- liftIO $ eitherStatusIO status404 $ Locations.lookupGeoHash postalCodeLookup postalCode
  radiusMiles <- lift $ (S.queryParam "radiusMiles" :: ActionM Int)
  days <- lift $ (S.queryParam "days" :: ActionM Int)
  startTime <- liftIO getCurrentTime
  let endTime = addUTCTime (fromIntegral days * nominalDay) startTime

  pageToken <- lift $ queryParamMaybe "pageToken"
  let pageNumberMaybe = readMaybe . LazyText.unpack . fromMaybe "0" $ pageToken :: Maybe Int
  pageNumber <- liftIO . maybeIO "failed to parse pageToken" $ pageNumberMaybe

  events <-
    liftIO $
      CreatePlaylist.searchEvents
        ticketmasterConsumerKey
        SearchEventsRequest.SearchEventsRequest
          { SearchEventsRequest.geoHash = T.pack . take 9 $ geoHash,
            SearchEventsRequest.radiusMiles = radiusMiles,
            SearchEventsRequest.classificationName = ["music"],
            SearchEventsRequest.startTime = startTime,
            SearchEventsRequest.endTime = endTime,
            SearchEventsRequest.pageSize = 200,
            SearchEventsRequest.pageNumber = pageNumber
          }
        50000

  lift $ S.addHeader "Cache-Control" "no-store"
  lift . S.json $ ListArtistsResponse.fromEvents events

getGeoHash :: App.AppT ActionM ()
getGeoHash = do
  postalCode <- lift (S.queryParam "postalCode" :: ActionM Text)
  postalCodeLookup <- App.postalCodeLookup <$> ask
  geoHash <- liftIO $ eitherStatusIO status404 $ Locations.lookupGeoHash postalCodeLookup postalCode
  lift $ S.addHeader "Cache-Control" "no-store"
  lift . S.text $ LazyText.pack geoHash

getPlaceName :: App.AppT ActionM ()
getPlaceName = do
  postalCode <- lift (S.queryParam "postalCode" :: ActionM Text)
  postalCodeLookup <- App.postalCodeLookup <$> ask
  placeName <- liftIO $ eitherStatusIO status404 $ Locations.lookupPlaceName postalCodeLookup postalCode
  lift $ S.addHeader "Cache-Control" "no-store"
  lift . S.text $ LazyText.fromStrict placeName

getTopArtists :: App.AppT ActionM ()
getTopArtists = do
  auth <- lift (S.queryParam "spotifyAccessToken" :: ActionM Text)
  limit <- lift (S.queryParam "limit" :: ActionM Int)
  artists <- liftIO $ CreatePlaylist.getTopArtists auth limit
  lift $ S.json artists

getTopTracks :: App.AppT ActionM ()
getTopTracks = do
  auth <- lift (S.queryParam "spotifyAccessToken" :: ActionM Text)
  limit <- lift (S.queryParam "limit" :: ActionM Int)
  tracks <- liftIO $ CreatePlaylist.getTopTracks auth limit
  lift $ S.json tracks

createDiscoveryJob :: App.AppT ActionM ()
createDiscoveryJob = do
  request <- lift $ S.jsonData :: App.AppT ActionM CreatePlaylistJobRequest.CreatePlaylistJobRequest
  let days = CreatePlaylistJobRequest.days request
  let postalCode = CreatePlaylistJobRequest.postalCode request
  let auth = CreatePlaylistJobRequest.spotifyAccessToken request
  let radiusMiles = CreatePlaylistJobRequest.radiusMiles request
  let spideringDepth = CreatePlaylistJobRequest.spideringDepth request

  postalCodeLookup <- App.postalCodeLookup <$> ask
  geoHash <- liftIO $ eitherStatusIO status404 $ Locations.lookupGeoHash postalCodeLookup postalCode
  startTime <- liftIO getCurrentTime
  let endTime = addUTCTime (fromIntegral days * nominalDay) startTime

  uuid <- liftIO . fmap UUID.toText $ UUIDV4.nextRandom
  let jobName = "discoveryJobs/" <> uuid

  let eventsReq =
        SearchEventsRequest.SearchEventsRequest
          { SearchEventsRequest.geoHash = T.pack . take 9 $ geoHash,
            SearchEventsRequest.radiusMiles = radiusMiles,
            SearchEventsRequest.classificationName = ["music"],
            SearchEventsRequest.startTime = startTime,
            SearchEventsRequest.endTime = endTime,
            SearchEventsRequest.pageSize = 200,
            SearchEventsRequest.pageNumber = 0
          }

  appState <- ask
  let discoveryCoroutine = CreatePlaylist.discoverSpotify appState auth eventsReq spideringDepth
  _ <- liftIO $ Jobs.runJob (App.jobsDB appState) jobName discoveryCoroutine

  lift . S.json $ object ["name" .= jobName]

getDiscoveryJob :: App.AppT ActionM ()
getDiscoveryJob = do
  jobId <- lift $ S.captureParam "id" :: App.AppT ActionM Text
  let jobName = "discoveryJobs/" <> jobId
  jobsDB <- App.jobsDB <$> ask
  job <- liftIO $ Jobs.getJob jobsDB jobName
  lift $ S.addHeader "Cache-Control" "no-store"
  lift . S.json $ job

createUser :: App.AppT ActionM ()
createUser = do
  App.runWithDB . Storage.upsertDiscoUser $ Storage.DiscoUser {Storage.discoUserUuid = "uuid-1234", Storage.discoUserSpotifyId = "spotify-5678"}
  lift $ S.text "ok"

getUser :: App.AppT ActionM ()
getUser = do
  user <- App.runWithDB $ Storage.getDiscoUser "uuid-1234"
  lift $ S.text $ LazyText.pack (fromMaybe "not found" (Storage.discoUserUuid <$> user))

allowCors :: Middleware
allowCors = cors (const $ Just appCorsResourcePolicy)

appCorsResourcePolicy :: CorsResourcePolicy
appCorsResourcePolicy =
  simpleCorsResourcePolicy
    { corsMethods = ["OPTIONS", "GET", "PUT", "POST"],
      corsRequestHeaders = ["content-type"]
    }

routes :: App.AppState -> IO ()
routes appState = S.scotty 8080 $ do
  S.defaultHandler (S.Handler handleException)
  S.middleware allowCors

  S.get "/spotify/clientId" $ App.runAppT appState getSpotifyClientId
  S.get "/spotify/topArtists" $ App.runAppT appState getTopArtists
  S.get "/spotify/topTracks" $ App.runAppT appState getTopTracks
  S.get "/geohash" $ App.runAppT appState getGeoHash
  S.get "/placeName" $ App.runAppT appState getPlaceName
  S.get "/localArtists" $ App.runAppT appState getEvents
  S.post "/discoveryJobs" $ App.runAppT appState createDiscoveryJob
  S.get "/discoveryJobs/:id" $ App.runAppT appState getDiscoveryJob
  S.get "/createUser" $ App.runAppT appState createUser
  S.get "/user" $ App.runAppT appState getUser
