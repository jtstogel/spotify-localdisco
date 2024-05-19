{-# LANGUAGE OverloadedStrings #-}

module CreatePlaylist
  ( getTopArtists
  , getTopTracks
  , searchEvents
  )
  where

import Data.List (reverse)
import Control.Monad (when)
import Data.Maybe (fromMaybe)
import qualified App as App
import qualified Spotify as Spotify
import qualified Types.Spotify.Artist as Artist
import qualified Types.Spotify.Track as Track
import qualified Types.Spotify.ListTopItemsRequest as ListTopItemsRequest
import qualified Types.Spotify.TopArtistsResponse as TopArtistsResponse
import qualified Types.Spotify.TopTracksResponse as TopTracksResponse
import qualified Types.Ticketmaster.SearchEventsResponse as SearchEventsResponse
import qualified Types.Ticketmaster.SearchEventsRequest as SearchEventsRequest
import qualified Types.Ticketmaster.Event as Event
import qualified Ticketmaster as Ticketmaster
import qualified Data.Text as T

data Playlist = Playlist
  { tracks :: [Track.Track]
  }

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

initialTopItemsRequest = ListTopItemsRequest.ListTopItemsRequest
  { ListTopItemsRequest.timeRange = Just "medium_term"
  , ListTopItemsRequest.offset = Just 0
  , ListTopItemsRequest.limit = Just 50 }

getTopArtists :: T.Text -> Int -> IO [Artist.Artist]
getTopArtists auth = listN (Spotify.listTopArtists auth) makeNextRequest getItems initialTopItemsRequest
    where
        getItems = fromMaybe [] . TopArtistsResponse.items
        makeNextRequest req res = do
            let count = length . fromMaybe [] . TopArtistsResponse.items $ res
            when (count == 0)
                mempty
            let offset = fromMaybe 0 $ TopArtistsResponse.offset res
            Just $ req { ListTopItemsRequest.offset = Just $ count + offset }

getTopTracks :: T.Text -> Int -> IO [Track.Track]
getTopTracks auth = listN (Spotify.listTopTracks auth) makeNextRequest getItems initialTopItemsRequest
    where
        getItems = fromMaybe [] . TopTracksResponse.items
        makeNextRequest req res = do
            let count = length . fromMaybe [] . TopTracksResponse.items $ res
            when (count == 0) Nothing
            let offset = fromMaybe 0 $ TopTracksResponse.offset res
            Just $ req { ListTopItemsRequest.offset = Just $ count + offset }

searchEvents :: T.Text -> SearchEventsRequest.SearchEventsRequest -> Int -> IO [Event.Event]
searchEvents auth = listN (Ticketmaster.searchEvents auth) makeNextRequest getItems
    where
        getItems = fromMaybe [] . SearchEventsResponse.events . SearchEventsResponse._embedded
        makeNextRequest req res = do
            let page = SearchEventsResponse.page res
            let pageNumber = SearchEventsResponse.number page
            let totalPages = SearchEventsResponse.totalPages page
            if ((pageNumber + 1) >= totalPages)
                then Nothing
                else Just $ req { SearchEventsRequest.pageNumber = pageNumber + 1 }


-- discoverTracks :: App.AppState -> Text -> Coroutine (Yield String) IO Playlist
-- discoverTracks appState spotifyAuth = do
--     topArtists <- Spotify.listTopArtists spotifyAuth ListTopItemsRequest
