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

export const apiSlice = createApi({
    baseQuery: fetchBaseQuery({
        baseUrl: "http://localhost:8080",
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
