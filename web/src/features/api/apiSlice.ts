import { createApi, fetchBaseQuery } from "@reduxjs/toolkit/query/react"

declare interface AuthenticateWithSpotifyRequest {
    code: string
    redirectUri: string
}

declare interface AuthenticateWithSpotifyResponse {
    userId: string
}

declare interface SpotifyUserProfile {
    displayName: string
    profileImageUrl: string
}

declare interface GetClientIdResponse {
    clientId: string
}

declare interface CreatePlaylistJobRequest {
    postalCode: string
    radiusMiles: number
    startTime: string
    endTime: string
    spideringDepth: number
    authToken: string
}

declare interface CreatePlaylistJobResponse {
    name: string
}

declare interface GetJobRequest {
    name: string
}

declare interface JobError {
    message: string
    code: number
}

declare interface SimpleStatusMetadata {
    message: string
}


declare interface GetJobResponse<Result, Metadata = SimpleStatusMetadata> {
    done: boolean
    error?: JobError
    result?: Result
    metadata?: Metadata
}

declare interface Playlist {
    artists: string[]
    playlistLink: string
}

function baseUri(): string {
    if (import.meta.env.PROD) {
        return 'https://api-disco.stowgulls.com'
    }
    return 'http://127.0.0.1:8080'
}

export function authorizationHeader(token: string) {
    return { 'Authorization': `Bearer ${token}` };
}

export const apiSlice = createApi({
    baseQuery: fetchBaseQuery({
        baseUrl: baseUri(),
        mode: 'cors',
    }),
    reducerPath: "api",
    keepUnusedDataFor: 5 * 60 * 60,
    tagTypes: ["Api"],
    endpoints: build => ({
        getSpotifyClientId: build.query<string, void>({
            query: () => ({
                url: '/spotify/clientId',
            }),
            transformResponse: ({ clientId }: GetClientIdResponse) => clientId,
        }),
        getSpotifyProfile: build.query<SpotifyUserProfile, string>({
            query: (token) => ({
                url: '/spotify/me',
                headers: { ...authorizationHeader(token) },
            }),
        }),
        authenticateWithSpotify: build.mutation<AuthenticateWithSpotifyResponse, AuthenticateWithSpotifyRequest>({
            query: (body) => ({
                url: '/spotify/authenticate',
                method: 'POST',
                body
            }),
        }),
        createPlaylistJob: build.mutation<CreatePlaylistJobResponse, CreatePlaylistJobRequest>({
            query: (body) => ({
                url: '/discoveryJobs',
                method: 'POST',
                headers: { ...authorizationHeader(body.authToken) },
                body
            })
        }),
        getPlaylistJob: build.query<GetJobResponse<Playlist>, GetJobRequest>({
            query: (req) => ({
                url: `/${req.name}`,
            }),
            keepUnusedDataFor: 0,
        })
    }),
})

export const { useGetSpotifyClientIdQuery, useGetSpotifyProfileQuery, useAuthenticateWithSpotifyMutation, useGetPlaylistJobQuery, useCreatePlaylistJobMutation } = apiSlice
