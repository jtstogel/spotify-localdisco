{-# LANGUAGE OverloadedStrings #-}

module CreatePlaylist
  ( discoverSpotify,
    getTopArtists,
    getTopTracks,
    searchEvents,
    recommendationsForTrack,
  )
where

import qualified App
import Control.Monad (when, (<=<))
import Control.Monad.Trans.Class (lift)
import Data.List (nub, sort)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Debug.Trace (traceShowId)
import qualified Jobs
import qualified Spotify
import qualified Ticketmaster
import qualified Types.Spotify.Artist as Artist
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

initialTopItemsRequest :: ListTopItemsRequest.ListTopItemsRequest
initialTopItemsRequest =
  ListTopItemsRequest.ListTopItemsRequest
    { ListTopItemsRequest.timeRange = Just "medium_term",
      ListTopItemsRequest.offset = Just 0,
      ListTopItemsRequest.limit = Just 50
    }

getTopArtists :: T.Text -> Int -> IO [Artist.Artist]
getTopArtists auth = listN (Spotify.listTopArtists auth) makeNextRequest getItems initialTopItemsRequest
  where
    getItems = fromMaybe [] . TopArtistsResponse.items
    makeNextRequest req res = do
      let count = length . fromMaybe [] . TopArtistsResponse.items $ res
      when (count == 0) Nothing
      let offset = fromMaybe 0 $ TopArtistsResponse.offset res
      Just $ req {ListTopItemsRequest.offset = Just $ count + offset}

getTopTracks :: T.Text -> Int -> IO [Track.Track]
getTopTracks auth = listN (Spotify.listTopTracks auth) makeNextRequest getItems initialTopItemsRequest
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

recommendationsForTrack :: T.Text -> Int -> Track.Track -> IO [Track.Track]
recommendationsForTrack auth limit track =
  getRecommendations
    auth
    ( traceShowId
        ( GetRecommendationsRequest.GetRecommendationsRequest
            { GetRecommendationsRequest.seedTracks = traceShowId [Track.id track],
              GetRecommendationsRequest.seedArtists = [],
              GetRecommendationsRequest.limit = limit
            }
        )
    )

recommendationsForArtist :: T.Text -> Int -> Artist.Artist -> IO [Track.Track]
recommendationsForArtist auth limit artist =
  getRecommendations
    auth
    ( GetRecommendationsRequest.GetRecommendationsRequest
        { GetRecommendationsRequest.seedArtists = [Artist.id artist],
          GetRecommendationsRequest.seedTracks = [],
          GetRecommendationsRequest.limit = limit
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
attractionNamesFromEvent = maybe [] (map Attraction.name) . Event.attractions . Event._embedded

discoverSpotify :: App.AppState -> T.Text -> SearchEventsRequest.SearchEventsRequest -> Jobs.Job SpotifyDiscovery.SpotifyDiscovery
discoverSpotify appState spotifyAuth eventsRequest = do
  Jobs.yieldStatus "Finding local events"
  -- You're only allowed to get 1000 events from the Ticketmaster API...
  events <- lift $ searchEvents (App.ticketmasterConsumerKey appState) eventsRequest 1000

  Jobs.yieldStatus "Getting your top artists"
  topArtists <- lift $ getTopArtists spotifyAuth 100

  Jobs.yieldStatus "Getting your top tracks"
  topTracks <- lift $ getTopTracks spotifyAuth 100

  Jobs.yieldStatus "Getting your followed artists"
  followedArtists <- lift $ getFollowedArtists spotifyAuth 100

  Jobs.yieldStatus "Getting your saved tracks"
  savedTracks <- lift $ getSavedTracks spotifyAuth 100

  Jobs.yieldStatus "Finding some new music based on your top tracks"
  tracksFromTopTracks <- lift $ mapM (recommendationsForTrack spotifyAuth 20) (take 20 topTracks)

  Jobs.yieldStatus "Finding some new music based on your top artists"
  tracksFromTopArtists <- lift $ mapM (recommendationsForArtist spotifyAuth 20) (take 20 topArtists)

  Jobs.yieldStatus "Putting it all together"
  let tracks = topTracks ++ savedTracks ++ concat tracksFromTopTracks ++ concat tracksFromTopArtists
  let artists = topArtists ++ followedArtists ++ concatMap (fromMaybe [] . Track.artists) tracks

  let spotifyArtists = nub . sort . map Artist.name $ artists
  let ticketmasterArtists = nub . sort . concatMap attractionNamesFromEvent $ events

  return $
    SpotifyDiscovery.SpotifyDiscovery
      { SpotifyDiscovery.artists = filter (`elem` spotifyArtists) ticketmasterArtists
      }
