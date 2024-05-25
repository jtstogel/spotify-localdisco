import { createApi, fetchBaseQuery } from "@reduxjs/toolkit/query/react"

declare interface GetClientIdResponse {
    clientId: string
}

declare interface CreatePlaylistJobRequest {
    postalCode: string
    spotifyAccessToken: string
    radiusMiles: number
    days: number
    spideringDepth: number
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
}

function baseUri(): string {
    if (import.meta.env.PROD) {
        return 'https://api-disco.stowgulls.com'
    }
    return 'http://127.0.0.1:8080'
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
        createPlaylistJob: build.mutation<CreatePlaylistJobResponse, CreatePlaylistJobRequest>({
            query: (body) => ({
                url: '/discoveryJobs',
                method: 'POST',
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

export const { useGetSpotifyClientIdQuery, useGetPlaylistJobQuery, useCreatePlaylistJobMutation } = apiSlice
