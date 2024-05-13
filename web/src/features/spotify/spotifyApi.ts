import React, { createContext, useContext } from "react"
import { get, post, promiseResolver, PromiseResolver } from "../api/api"

declare interface GetClientIdResponse {
  clientId: string
}

export async function getClientId(): Promise<string> {
  const response = await get<GetClientIdResponse>("/spotify/clientId")
  return response.clientId
}

declare interface AuthenticateRequest {
  code: string
  redirectUri: string
}

declare interface AuthenticateResponse {
  expiresIn: number
  accessToken: string
  refreshToken: string
}

export async function exchangeAuthToken(
  code: string,
  redirectUri: string,
): Promise<AuthenticateResponse> {
  const request: AuthenticateRequest = { code, redirectUri }
  return post<AuthenticateResponse>(
    "/spotify/authenticate",
    JSON.stringify(request),
  )
}

declare interface SpotifyUserProfile {
  displayName: string
  profileImageUrl?: string
}

export async function getUserProfile(
  accessToken: string,
): Promise<SpotifyUserProfile> {
  return get<SpotifyUserProfile>("/spotify/users/me", [["accessToken", accessToken]])
}
