import { get } from "../api/api"
import { createApi, fetchBaseQuery } from "@reduxjs/toolkit/query/react"

declare interface GetClientIdResponse {
  clientId: string
}

export declare interface AuthenticateRequest {
  code: string
  redirectUri: string
}

export declare interface AuthenticateResponse {
  expiresIn: number
  accessToken: string
  refreshToken: string
}

export interface AccessToken {
  accessToken: string;
}

export declare interface SpotifyUserProfile {
  displayName: string
  profileImageUrl?: string
}

export async function getUserProfile(
  accessToken: string,
): Promise<SpotifyUserProfile> {
  return get<SpotifyUserProfile>("/spotify/users/me", [["accessToken", accessToken]])
}

export const spotifyApiSlice = createApi({
  baseQuery: fetchBaseQuery({
    baseUrl: "http://localhost:8080",
    mode: 'cors',
  }),
  reducerPath: "spotifyApi",
  keepUnusedDataFor: 5 * 60 * 60,
  tagTypes: ["Spotify"],
  endpoints: build => ({
    getClientId: build.query<string, void>({
      query: () => '/spotify/clientId',
      transformResponse: (response: GetClientIdResponse) => response.clientId,
    }),
    getUserProfile: build.query<SpotifyUserProfile, AccessToken>({
      query: ({ accessToken }) => `/spotify/users/me?accessToken=${accessToken}`,
    }),
    exchangeCode: build.mutation<AuthenticateResponse, AuthenticateRequest>({
      query: (body: AuthenticateRequest) => ({
        url: '/spotify/authenticate',
        method: 'POST',
        body,
      }),
    }),
  }),
})

export const { useGetClientIdQuery, useExchangeCodeMutation, useGetUserProfileQuery } = spotifyApiSlice
