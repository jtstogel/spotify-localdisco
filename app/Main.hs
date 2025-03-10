{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified App
import qualified CommandLineArgs
import Control.Exception (SomeException (..), fromException)
import Control.Monad (when, (<=<))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Loops (untilJust)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ask, asks)
import qualified CreatePlaylist
import Data.Aeson (KeyValue ((.=)), eitherDecode, object)
import qualified Data.ByteString.Lazy as B
import Data.Maybe (fromJust, isNothing)
import Data.Text (Text, stripPrefix)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Text.Lazy as LazyText
import Data.Time.Clock (UTCTime, addUTCTime, getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Time.Format.ISO8601 (iso8601ParseM)
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUIDV4
import qualified Durations
import Env (Env (spotifyClientID))
import qualified Env
import Errors (ErrStatus (..), eitherIO, eitherStatusIO, maybeStatusIO, throwErr)
import qualified Jobs
import qualified Locations
import Network.HTTP.Types.Status (Status (statusMessage), status400, status403, status404, status500)
import Network.Wai (Middleware)
import Network.Wai.Middleware.Cors (CorsResourcePolicy (..), cors, simpleCorsResourcePolicy)
import qualified Spotify
import Storage (DiscoUser)
import qualified Storage
import qualified Types.AuthenticateWithSpotifyRequest as AuthenticateWithSpotifyRequest
import qualified Types.AuthenticateWithSpotifyResponse as AuthenticateWithSpotifyResponse
import qualified Types.CreatePlaylistJobRequest as CreatePlaylistJobRequest
import qualified Types.GetSpotifyClientIdResponse as GetSpotifyClientIdResponse
import qualified Types.Spotify.AuthenticateResponse as SpotifyAuthenticateResponse
import qualified Types.Spotify.UserProfile as SpotifyUserProfile
import qualified Types.SpotifyProfile as SpotifyProfile
import qualified Types.Ticketmaster.SearchEventsRequest as SearchEventsRequest
import Utils (maybeHead)
import Web.Scotty (ActionM)
import qualified Web.Scotty as S

main :: IO ()
main = do
  args <- CommandLineArgs.parse
  env <- Env.load
  postalCodeLookup <- loadPostalCodeLookup (T.unpack $ CommandLineArgs.postalCodesPath args)
  jobsDB <- Jobs.newDB
  dbHandle <- Storage.createDatabaseHandle (CommandLineArgs.databasePath args) 10

  let appState =
        App.AppState
          { App.spotifyClientID = Env.spotifyClientID env,
            App.spotifyClientSecret = Env.spotifyClientSecret env,
            App.ticketmasterConsumerKey = Env.ticketmasterConsumerKey env,
            App.postalCodeLookup = postalCodeLookup,
            App.jobsDB = jobsDB,
            App.dbHandle = dbHandle,
            App.port = CommandLineArgs.port args
          }

  routes appState

loadPostalCodeLookup :: String -> IO Locations.PostalCodeLookup
loadPostalCodeLookup filename = do
  content <- B.readFile filename
  entries <- eitherIO (eitherDecode content :: Either String [Locations.PostalCodeLocation])
  return $ Locations.buildPostalCodeLookup entries

handleException :: SomeException -> ActionM ()
handleException e = do
  let maybeErr = fromException e :: Maybe ErrStatus
  case maybeErr of
    Just (ErrStatus s msg) -> do
      S.status s
      S.json $ object ["error" .= (T.pack msg <> ": " <> decodeUtf8 (statusMessage s))]
    Nothing -> do
      S.status status500
      S.json $ object ["error" .= T.pack (show e)]

getSpotifyClientId :: App.AppT ActionM ()
getSpotifyClientId = do
  lift $ S.addHeader "Cache-Control" "no-store"
  clientId <- App.spotifyClientID <$> ask
  lift . S.json $
    GetSpotifyClientIdResponse.GetSpotifyClientIdResponse
      { GetSpotifyClientIdResponse.clientId = clientId
      }

getPlaceName :: App.AppT ActionM ()
getPlaceName = do
  postalCode <- lift (S.queryParam "postalCode" :: ActionM Text)
  postalCodeLookup <- App.postalCodeLookup <$> ask
  placeName <- liftIO $ eitherStatusIO status404 $ Locations.lookupPlaceName postalCodeLookup postalCode
  lift $ S.addHeader "Cache-Control" "no-store"
  lift . S.text $ LazyText.fromStrict placeName

mkPlaylistName :: Text -> UTCTime -> UTCTime -> Text
mkPlaylistName placeName start end = "Concerts near " <> placeName <> ", " <> dateRange
  where
    month = T.pack . formatTime defaultTimeLocale "%b"
    year = T.pack . formatTime defaultTimeLocale "%Y"
    sameMonth = month start == month end
    sameYear = year start == year end
    dateRange
      -- Jan 2024
      | sameMonth && sameYear = month end <> " " <> year end
      -- Jan-Aug 2024
      | not sameMonth && sameYear = month start <> "-" <> month end <> " " <> year end
      -- Nov 2024 - Feb 2025
      | otherwise = month start <> " " <> year start <> " - " <> month end <> " " <> year end

createDiscoveryJob :: App.AppT ActionM ()
createDiscoveryJob = do
  user <- getAuthenticatedUserOrFail
  auth <- getSpotifyAccessToken user

  request <- lift S.jsonData :: App.AppT ActionM CreatePlaylistJobRequest.CreatePlaylistJobRequest
  startTime <- liftIO $ maybeStatusIO status400 "failed to parse startTime" $ (iso8601ParseM $ CreatePlaylistJobRequest.startTime request :: Maybe UTCTime)
  endTime <- liftIO $ maybeStatusIO status400 "failed to parse startTime" $ (iso8601ParseM $ CreatePlaylistJobRequest.endTime request :: Maybe UTCTime)
  let postalCode = CreatePlaylistJobRequest.postalCode request
  let radiusMiles = CreatePlaylistJobRequest.radiusMiles request
  let spideringDepth = CreatePlaylistJobRequest.spideringDepth request

  postalCodeLookup <- App.postalCodeLookup <$> ask
  geoHash <- liftIO $ eitherStatusIO status404 $ Locations.lookupGeoHash postalCodeLookup postalCode

  placeName <- liftIO $ eitherStatusIO status404 $ Locations.lookupPlaceName postalCodeLookup postalCode
  let playlistName = mkPlaylistName placeName startTime endTime

  uuid <- liftIO . fmap UUID.toText $ UUIDV4.nextRandom
  let jobName = "discoveryJobs/" <> uuid

  let eventsReq =
        SearchEventsRequest.SearchEventsRequest
          { SearchEventsRequest.geoHash = T.pack geoHash,
            SearchEventsRequest.radiusMiles = radiusMiles,
            SearchEventsRequest.classificationName = ["music"],
            SearchEventsRequest.startTime = startTime,
            SearchEventsRequest.endTime = endTime,
            SearchEventsRequest.pageSize = 200,
            SearchEventsRequest.pageNumber = 0
          }

  appState <- ask
  let discoveryCoroutine = CreatePlaylist.discoverSpotify appState auth (Storage.discoUserSpotifyId user) eventsReq spideringDepth playlistName
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

exchangeAuthCode :: AuthenticateWithSpotifyRequest.AuthenticateWithSpotifyRequest -> App.AppT ActionM SpotifyAuthenticateResponse.AuthenticateResponse
exchangeAuthCode req = do
  clientId <- asks App.spotifyClientID
  clientSecret <- asks App.spotifyClientSecret
  let code = AuthenticateWithSpotifyRequest.code req
  let rediectUri = AuthenticateWithSpotifyRequest.redirectUri req
  liftIO $ Spotify.exchangeAuthCode clientId clientSecret rediectUri code

getOrCreateUser :: (MonadUnliftIO m) => Text -> Storage.DatabaseT m DiscoUser
getOrCreateUser spotifyUserId = do
  maybeUser <- Storage.getDiscoUserBySpotifyId spotifyUserId
  case maybeUser of
    Just user -> return user
    Nothing -> do
      userId <- liftIO . fmap UUID.toText $ UUIDV4.nextRandom
      let user = Storage.DiscoUser {Storage.discoUserUuid = userId, Storage.discoUserSpotifyId = spotifyUserId}
      Storage.upsertDiscoUser user
      return user

saveSpotifyAuth :: (MonadUnliftIO m) => Text -> SpotifyAuthenticateResponse.AuthenticateResponse -> Storage.DatabaseT m DiscoUser
saveSpotifyAuth spotifyUserId authResponse = do
  now <- liftIO getCurrentTime
  let authExpireTime = addUTCTime (fromIntegral . SpotifyAuthenticateResponse.expires_in $ authResponse) now
  when (isNothing . SpotifyAuthenticateResponse.refresh_token $ authResponse) $
    liftIO $
      throwErr status500 "didn't get a refresh token from spotify"

  Storage.upsertSpotifyUserAuth
    Storage.SpotifyUserAuth
      { Storage.spotifyUserAuthSpotifyUserId = spotifyUserId,
        Storage.spotifyUserAuthAccessToken = SpotifyAuthenticateResponse.access_token $ authResponse,
        Storage.spotifyUserAuthRefreshToken = fromJust . SpotifyAuthenticateResponse.refresh_token $ authResponse,
        Storage.spotifyUserAuthExpireTime = authExpireTime,
        Storage.spotifyUserAuthRefreshing = False
      }
  getOrCreateUser spotifyUserId

authenticateWithSpotify :: App.AppT ActionM ()
authenticateWithSpotify = do
  request <- lift S.jsonData :: App.AppT ActionM AuthenticateWithSpotifyRequest.AuthenticateWithSpotifyRequest
  authResponse <- exchangeAuthCode request

  userProfile <- liftIO . Spotify.getUserProfile . SpotifyAuthenticateResponse.access_token $ authResponse
  let spotifyUserId = SpotifyUserProfile.id userProfile

  user <- App.runWithDB $ saveSpotifyAuth spotifyUserId authResponse
  lift . S.json $
    AuthenticateWithSpotifyResponse.AuthenticateWithSpotifyResponse
      { AuthenticateWithSpotifyResponse.userId = Storage.discoUserUuid user
      }

getAuthenticatedUser :: App.AppT ActionM (Maybe Storage.DiscoUser)
getAuthenticatedUser = do
  authorization <- lift . S.header $ "Authorization"
  let userIdMaybe = (>>= stripPrefix "Bearer ") $ LazyText.toStrict <$> authorization
  case userIdMaybe of
    Nothing -> return Nothing
    Just userId -> App.runWithDB $ Storage.getDiscoUser userId

getAuthenticatedUserOrFail :: App.AppT ActionM Storage.DiscoUser
getAuthenticatedUserOrFail = do
  userMaybe <- getAuthenticatedUser
  case userMaybe of
    Nothing -> liftIO $ throwErr status403 "could not verify authenticated user"
    Just user -> return user

getAuthWithRefreshLock :: (MonadUnliftIO m) => Storage.DiscoUser -> UTCTime -> App.AppT m (Storage.SpotifyUserAuth, Bool)
getAuthWithRefreshLock user deadline = untilJust $ do
  now <- liftIO getCurrentTime
  when (now > deadline) $ do
    _ <- App.runWithDB $ Storage.deleteSpotifyUserAuthByUser user
    liftIO $ throwErr status403 "could not refresh spotify credentials"

  App.runWithDB $ do
    authMaybe <- Storage.getSpotifyUserAuthByUser user
    auth <- liftIO $ maybeStatusIO status403 "no spotify credentials saved for the authenticated user" authMaybe

    if Storage.spotifyUserAuthRefreshing auth
      then return Nothing
      else do
        -- Require that the auth token is valid for 10m.
        let buffer = 10 * Durations.minute
        let isValid = Storage.spotifyUserAuthExpireTime auth > addUTCTime buffer now
        if isValid
          then return $ Just (auth, False)
          else do
            Storage.upsertSpotifyUserAuth $ auth {Storage.spotifyUserAuthRefreshing = True}
            return $ Just (auth, True)

getSpotifyAccessToken :: (MonadUnliftIO m) => Storage.DiscoUser -> App.AppT m Text
getSpotifyAccessToken user = do
  now <- liftIO getCurrentTime
  let deadline = addUTCTime 5 now
  (auth, refresh) <- getAuthWithRefreshLock user deadline
  if not refresh
    then return $ Storage.spotifyUserAuthAccessToken auth
    else do
      clientId <- asks App.spotifyClientID
      clientSecret <- asks App.spotifyClientSecret
      let refreshToken = Storage.spotifyUserAuthRefreshToken auth
      authResponse <- liftIO $ Spotify.refreshAuthToken clientId clientSecret refreshToken
      let authResponseWithRefreshToken = authResponse {SpotifyAuthenticateResponse.refresh_token = Just refreshToken}
      _ <- App.runWithDB $ saveSpotifyAuth (Storage.discoUserSpotifyId user) authResponseWithRefreshToken
      return $ SpotifyAuthenticateResponse.access_token authResponse

getSpotifyProfile :: App.AppT ActionM ()
getSpotifyProfile = do
  user <- getAuthenticatedUserOrFail
  accessToken <- getSpotifyAccessToken user
  profile <- liftIO $ Spotify.getUserProfile accessToken
  let profileImage = maybe "" SpotifyUserProfile.url . (maybeHead <=< SpotifyUserProfile.images) $ profile
  lift . S.json $
    SpotifyProfile.SpotifyProfile
      { SpotifyProfile.displayName = SpotifyUserProfile.display_name profile,
        SpotifyProfile.profileImageUrl = profileImage
      }

allowCors :: Middleware
allowCors = cors (const $ Just appCorsResourcePolicy)

appCorsResourcePolicy :: CorsResourcePolicy
appCorsResourcePolicy =
  simpleCorsResourcePolicy
    { corsMethods = ["OPTIONS", "GET", "PUT", "POST"],
      corsRequestHeaders = ["content-type", "authorization"]
    }

routes :: App.AppState -> IO ()
routes appState = S.scotty (App.port appState) $ do
  S.defaultHandler (S.Handler handleException)
  S.middleware allowCors

  S.post "/spotify/authenticate" $ App.runAppT appState authenticateWithSpotify
  S.get "/spotify/me" $ App.runAppT appState getSpotifyProfile
  S.get "/spotify/clientId" $ App.runAppT appState getSpotifyClientId

  S.get "/placeName" $ App.runAppT appState getPlaceName
  S.post "/discoveryJobs" $ App.runAppT appState createDiscoveryJob
  S.get "/discoveryJobs/:id" $ App.runAppT appState getDiscoveryJob
