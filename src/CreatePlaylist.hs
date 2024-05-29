{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module CreatePlaylist
  ( discoverSpotify,
    getTopArtists,
    getTopTracks,
    searchEvents,
    recommendationsForTrack,
    SpotifyTermDuration (..),
  )
where

import qualified App
import Control.Monad (forM, when, (<=<))
import Control.Monad.Trans.Class (lift)
import Data.List (foldl', nub, nubBy, sort)
import qualified Data.Map as M
import Data.Map.Strict (insertWith)
import Data.Maybe (fromMaybe, mapMaybe)
import qualified Data.Text as T
import Data.Time (NominalDiffTime, UTCTime, addUTCTime)
import qualified Jobs
import qualified Spotify
import qualified Ticketmaster
import qualified Types.Spotify.AddTracksRequest as AddTracksRequest
import qualified Types.Spotify.Artist as Artist
import qualified Types.Spotify.CreatePlaylistRequest as CreatePlaylistRequest
import qualified Types.Spotify.CreatePlaylistResponse as CreatePlaylistResponse
import qualified Types.Spotify.GetArtistTopTracksRequest as GetArtistTopTracksRequest
import qualified Types.Spotify.GetArtistTopTracksResponse as GetArtistTopTracksResponse
import qualified Types.Spotify.GetRecommendationsRequest as GetRecommendationsRequest
import qualified Types.Spotify.GetRecommendationsResponse as GetRecommendationsResponse
import qualified Types.Spotify.ListFollowedArtistsRequest as ListFollowedArtistsRequest
import qualified Types.Spotify.ListFollowedArtistsResponse as ListFollowedArtistsResponse
import qualified Types.Spotify.ListSavedTracksRequest as ListSavedTracksRequest
import qualified Types.Spotify.ListSavedTracksResponse as ListSavedTracksResponse
import qualified Types.Spotify.ListTopItemsRequest as ListTopItemsRequest
import qualified Types.Spotify.TopArtistsResponse as TopArtistsResponse
import qualified Types.Spotify.TopTracksResponse as TopTracksResponse
import qualified Types.Spotify.Track as Track
import qualified Types.SpotifyDiscovery as SpotifyDiscovery
import qualified Types.Ticketmaster.Attraction as Attraction
import qualified Types.Ticketmaster.Event as Event
import qualified Types.Ticketmaster.SearchEventsRequest as SearchEventsRequest
import qualified Types.Ticketmaster.SearchEventsResponse as SearchEventsResponse
import Utils (chunks)

-- Gets n items from a List API.
listN :: (Monad m) => (req -> m res) -> (req -> res -> Maybe req) -> (res -> [item]) -> req -> Int -> m [item]
listN list makeNextRequest getItems req n = do
  res <- list req
  let items = getItems res
  let count = length items

  if count >= n
    then return . take n $ items
    else case makeNextRequest req res of
      Nothing -> return items
      Just nextReq -> do
        remaining <- listN list makeNextRequest getItems nextReq (n - count)
        return $ items ++ remaining

data SpotifyTermDuration = LongTerm | MediumTerm | ShortTerm

instance Show SpotifyTermDuration where
  show :: SpotifyTermDuration -> String
  show LongTerm = "long_term"
  show MediumTerm = "medium_term"
  show ShortTerm = "short_term"

initialTopItemsRequest :: SpotifyTermDuration -> ListTopItemsRequest.ListTopItemsRequest
initialTopItemsRequest term =
  ListTopItemsRequest.ListTopItemsRequest
    { ListTopItemsRequest.timeRange = Just . T.pack $ show term,
      ListTopItemsRequest.offset = Just 0,
      ListTopItemsRequest.limit = Just 50
    }

getTopArtists :: T.Text -> SpotifyTermDuration -> Int -> IO [Artist.Artist]
getTopArtists auth term = listN (Spotify.listTopArtists auth) makeNextRequest getItems (initialTopItemsRequest term)
  where
    getItems = fromMaybe [] . TopArtistsResponse.items
    makeNextRequest req res = do
      let count = length . fromMaybe [] . TopArtistsResponse.items $ res
      when (count == 0) Nothing
      let offset = fromMaybe 0 $ TopArtistsResponse.offset res
      Just $ req {ListTopItemsRequest.offset = Just $ count + offset}

getTopTracks :: T.Text -> SpotifyTermDuration -> Int -> IO [Track.Track]
getTopTracks auth term = listN (Spotify.listTopTracks auth) makeNextRequest getItems (initialTopItemsRequest term)
  where
    getItems = fromMaybe [] . TopTracksResponse.items
    makeNextRequest req res = do
      let count = length . getItems $ res
      when (count == 0) Nothing
      let offset = fromMaybe 0 $ TopTracksResponse.offset res
      Just $ req {ListTopItemsRequest.offset = Just $ count + offset}

initialSavedTracksRequest :: ListSavedTracksRequest.ListSavedTracksRequest
initialSavedTracksRequest =
  ListSavedTracksRequest.ListSavedTracksRequest
    { ListSavedTracksRequest.limit = Just 50,
      ListSavedTracksRequest.offset = Nothing
    }

getSavedTracks :: T.Text -> Int -> IO [Track.Track]
getSavedTracks auth = listN (Spotify.listSavedTracks auth) makeNextRequest getItems initialSavedTracksRequest
  where
    getItems = maybe [] (map ListSavedTracksResponse.track) . ListSavedTracksResponse.items
    makeNextRequest req res = do
      let count = length . getItems $ res
      when (count == 0) Nothing
      let offset = fromMaybe 0 $ ListSavedTracksResponse.offset res
      Just $ req {ListSavedTracksRequest.offset = Just $ count + offset}

initialFollowedArtistsRequest :: ListFollowedArtistsRequest.ListFollowedArtistsRequest
initialFollowedArtistsRequest =
  ListFollowedArtistsRequest.ListFollowedArtistsRequest
    { ListFollowedArtistsRequest.after = Nothing,
      ListFollowedArtistsRequest.limit = Just 50
    }

getFollowedArtists :: T.Text -> Int -> IO [Artist.Artist]
getFollowedArtists auth = listN (Spotify.listFollowedArtists auth) makeNextRequest getItems initialFollowedArtistsRequest
  where
    getItems = fromMaybe [] . (ListFollowedArtistsResponse.items <=< ListFollowedArtistsResponse.artists)
    makeNextRequest req res = do
      artists <- ListFollowedArtistsResponse.artists res
      cursor <- ListFollowedArtistsResponse.cursor artists
      after <- ListFollowedArtistsResponse.after cursor
      when (after == "") Nothing
      Just $ req {ListFollowedArtistsRequest.after = Just after}

getRecommendations :: T.Text -> GetRecommendationsRequest.GetRecommendationsRequest -> IO [Track.Track]
getRecommendations auth = fmap GetRecommendationsResponse.tracks . Spotify.getRecommendations auth

recommendationsForTrack :: T.Text -> Track.Track -> IO [Track.Track]
recommendationsForTrack auth track =
  getRecommendations
    auth
    ( GetRecommendationsRequest.GetRecommendationsRequest
        { GetRecommendationsRequest.seedTracks = [Track.id track],
          GetRecommendationsRequest.seedArtists = [],
          GetRecommendationsRequest.limit = 20
        }
    )

recommendationsForArtist :: T.Text -> Artist.Artist -> IO [Track.Track]
recommendationsForArtist auth artist =
  getRecommendations
    auth
    ( GetRecommendationsRequest.GetRecommendationsRequest
        { GetRecommendationsRequest.seedArtists = [Artist.id artist],
          GetRecommendationsRequest.seedTracks = [],
          GetRecommendationsRequest.limit = 20
        }
    )

searchEvents :: T.Text -> SearchEventsRequest.SearchEventsRequest -> Int -> IO [Event.Event]
searchEvents auth = listN (Ticketmaster.searchEvents auth) makeNextRequest getItems
  where
    getItems = fromMaybe [] . SearchEventsResponse.events . SearchEventsResponse._embedded
    makeNextRequest req res = do
      let page = SearchEventsResponse.page res
      let pageNumber = SearchEventsResponse.number page
      let totalPages = SearchEventsResponse.totalPages page
      if pageNumber + 1 >= totalPages
        then Nothing
        else Just $ req {SearchEventsRequest.pageNumber = pageNumber + 1}

attractionNamesFromEvent :: Event.Event -> [T.Text]
attractionNamesFromEvent = maybe [] (mapMaybe Attraction.name) . Event.attractions . Event._embedded

repeatedMap :: (Ord k) => [(k, v)] -> M.Map k [v]
repeatedMap = foldl' (\m (k, v) -> insertWith (++) k [v] m) M.empty

nubUsing :: (Eq a) => (e -> a) -> [e] -> [e]
nubUsing f = nubBy (\a b -> f a == f b)

intervals :: (UTCTime, UTCTime) -> NominalDiffTime -> [(UTCTime, UTCTime)]
intervals (start, end) maxDuration
  | maxEndTime < end = (start, maxEndTime) : intervals (maxEndTime, end) maxDuration
  | otherwise = [(start, end)]
  where
    maxEndTime = addUTCTime maxDuration start

discoverSpotify :: App.AppState -> T.Text -> T.Text -> SearchEventsRequest.SearchEventsRequest -> Int -> T.Text -> Jobs.Job SpotifyDiscovery.SpotifyDiscovery
discoverSpotify appState spotifyAuth spotifyUserId eventsRequest spideringDepth playlistName = do
  Jobs.yieldStatus "Finding local events"
  -- You're only allowed to get 1000 events from the Ticketmaster API..
  -- So break up the search requests into 3 week chunks.
  let eventStartTime = SearchEventsRequest.startTime eventsRequest
  let eventEndTime = SearchEventsRequest.endTime eventsRequest
  let maxInterval = fromInteger (3 * 7 * 24 * 60 * 60) :: NominalDiffTime
  eventResponses <- forM (intervals (eventStartTime, eventEndTime) maxInterval) $ \(start, end) -> do
    let req = eventsRequest {SearchEventsRequest.startTime = start, SearchEventsRequest.endTime = end}
    lift $ searchEvents (App.ticketmasterConsumerKey appState) req 1000
  let events = concat eventResponses

  Jobs.yieldStatus "Getting your top artists"
  topArtists <- lift $ do
    long <- getTopArtists spotifyAuth LongTerm 1000
    medium <- getTopArtists spotifyAuth MediumTerm 1000
    short <- getTopArtists spotifyAuth ShortTerm 1000
    return $ long ++ medium ++ short

  Jobs.yieldStatus "Getting your top tracks"
  topTracks <- lift $ do
    long <- getTopTracks spotifyAuth LongTerm 1000
    medium <- getTopTracks spotifyAuth MediumTerm 1000
    short <- getTopTracks spotifyAuth ShortTerm 1000
    return $ long ++ medium ++ short

  Jobs.yieldStatus "Getting your followed artists"
  followedArtists <- lift $ getFollowedArtists spotifyAuth 5000

  Jobs.yieldStatus "Getting your saved tracks"
  savedTracks <- lift $ getSavedTracks spotifyAuth 5000

  Jobs.yieldStatus "Finding some new music based on your top tracks"
  tracksFromTopTracks <- lift $ mapM (recommendationsForTrack spotifyAuth) (take spideringDepth topTracks)

  Jobs.yieldStatus "Finding some new music based on your top artists"
  tracksFromTopArtists <- lift $ mapM (recommendationsForArtist spotifyAuth) (take spideringDepth topArtists)

  let tracks = nubUsing Track.id $ topTracks ++ savedTracks ++ concat tracksFromTopTracks ++ concat tracksFromTopArtists
  let artists = nubUsing Artist.id $ topArtists ++ followedArtists ++ concatMap (fromMaybe [] . Track.artists) tracks

  let ticketmasterArtists = nub . sort . concatMap attractionNamesFromEvent $ events

  let localArtists = filter (\a -> Artist.name a `elem` ticketmasterArtists) artists
  let tracksByArtistName = repeatedMap $ concatMap (\t -> maybe [] (map (\a -> (Artist.name a, t))) (Track.artists t)) tracks :: M.Map T.Text [Track.Track]

  artistsWithTracks <- forM localArtists $ \artist -> do
    Jobs.yieldStatus $ "Getting tracks by " <> Artist.name artist
    let knownTracks = fromMaybe [] $ M.lookup (Artist.name artist) tracksByArtistName
    if length knownTracks >= 3
      then return (artist, take 3 knownTracks)
      else do
        response <- lift $ Spotify.artistTopTracks spotifyAuth $ GetArtistTopTracksRequest.GetArtistTopTracksRequest {GetArtistTopTracksRequest.artistId = Artist.id artist}
        -- Prefer the tracks that Spotify recommended.
        let allTracks = nubUsing Track.id $ knownTracks ++ GetArtistTopTracksResponse.tracks response
        return (artist, take 3 allTracks)

  Jobs.yieldStatus "Creating your playlist"
  playlistResponse <-
    lift $
      Spotify.createPlaylist spotifyAuth spotifyUserId $
        CreatePlaylistRequest.CreatePlaylistRequest
          { CreatePlaylistRequest.name = playlistName,
            CreatePlaylistRequest.description = "Exclude this playlist from your taste profile to avoid messing up future recs.",
            CreatePlaylistRequest.public = False
          }
  let playlistId = CreatePlaylistResponse.id playlistResponse

  Jobs.yieldStatus "Adding tracks to your playlist"
  _ <- lift $ forM (chunks 100 $ concatMap snd artistsWithTracks) $ \tracksChunk -> do
    _ <- Spotify.addTracks spotifyAuth playlistId $ AddTracksRequest.AddTracksRequest {AddTracksRequest.uris = map Track.uri tracksChunk}
    return ()

  return $
    SpotifyDiscovery.SpotifyDiscovery
      { SpotifyDiscovery.playlistLink = CreatePlaylistResponse.spotify $ CreatePlaylistResponse.external_urls playlistResponse,
        SpotifyDiscovery.artists = map (Artist.name . fst) artistsWithTracks,
        SpotifyDiscovery.spotifyArtists = map Artist.name artists,
        SpotifyDiscovery.ticketmasterArtists = ticketmasterArtists
      }
