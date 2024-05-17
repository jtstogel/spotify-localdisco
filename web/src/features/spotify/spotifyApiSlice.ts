import { createApi, fetchBaseQuery } from "@reduxjs/toolkit/query/react"
import type { SpotifyAuthToken} from "./spotifyAccountsApiSlice";
import { authorizationHeader } from "./spotifyAccountsApiSlice";

declare interface ProfileImage {
    url: string;
}

export declare interface SpotifyUserProfile {
    displayName: string
    images?: ProfileImage[];
}

export const spotifyApiSlice = createApi({
    baseQuery: fetchBaseQuery({
        baseUrl: "https://api.spotify.com/v1",
        mode: 'cors',
    }),
    reducerPath: "spotifyApi",
    keepUnusedDataFor: 5 * 60 * 60,
    tagTypes: ["Spotify"],
    endpoints: build => ({
        getMyProfile: build.query<SpotifyUserProfile, SpotifyAuthToken>({
            query: (token) => ({
                url: '/me',
                headers: { ...authorizationHeader(token) },
            }),
            transformResponse: (response: Record<string, any>) => ({
                displayName: response['display_name'],
                images: response['images']?.map((image: Record<string, any>) => ({
                    url: image['url'],
                })),
            }),
        }),
    }),
})

export const { useGetMyProfileQuery } = spotifyApiSlice
