import { createApi, fetchBaseQuery } from "@reduxjs/toolkit/query/react"

declare interface GetClientIdResponse {
    clientId: string
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
    }),
})

export const { useGetSpotifyClientIdQuery } = apiSlice
