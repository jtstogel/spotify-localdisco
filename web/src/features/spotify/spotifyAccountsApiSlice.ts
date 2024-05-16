import { createApi, fetchBaseQuery } from "@reduxjs/toolkit/query/react"

export interface AuthenticateRequest {
  code: string;
  redirectUri: string;
  clientId: string;
  codeVerifier: string;
}

export interface RefreshAuthenticationRequest {
  refreshToken: string;
  clientId: string;
}

export interface SpotifyAuthToken {
  accessToken: string;
  refreshToken: string;
  tokenType: string;
  expireTime: string;
  scope: string;
}

export function authorizationHeader(token: SpotifyAuthToken) {
  return {'Authorization': `${token.tokenType} ${token.accessToken}`};
}

function parseAuthToken(response: Record<string, any>): SpotifyAuthToken {
  const expiresIn: number = response['expires_in'];
  const expireTime = new Date((expiresIn - 10) * 1000 + Date.now()).toISOString();
  return {
    accessToken: response['access_token'],
    refreshToken: response['refresh_token'],
    scope: response['scope'],
    tokenType: response['token_type'],
    expireTime,
  }
}

export const spotifyAccountsApiSlice = createApi({
  baseQuery: fetchBaseQuery({
    baseUrl: "https://accounts.spotify.com",
    mode: 'cors',
  }),
  reducerPath: "spotifyAccountsApi",
  keepUnusedDataFor: 5 * 60 * 60,
  tagTypes: ["Spotify"],
  endpoints: build => ({
    exchangeCode: build.mutation<SpotifyAuthToken, AuthenticateRequest>({
      query: (req: AuthenticateRequest) => ({
        url: '/api/token',
        method: 'POST',
        headers: { 'Content-Type': 'application/x-www-form-urlencoded' },
        body: new URLSearchParams({
          'grant_type': 'authorization_code',
          'client_id': req.clientId,
          'code': req.code,
          'redirect_uri': req.redirectUri,
          'code_verifier': req.codeVerifier,
        }).toString(),
      }),
      transformResponse: parseAuthToken,
    }),
    refreshAccess: build.mutation<SpotifyAuthToken, RefreshAuthenticationRequest>({
      query: (req: RefreshAuthenticationRequest) => ({
        url: '/api/token',
        method: 'POST',
        headers: { 'Content-Type': 'application/x-www-form-urlencoded' },
        body: new URLSearchParams({
          'grant_type': 'refresh_token',
          'client_id': req.clientId,
          'refresh_token': req.refreshToken,
        }).toString(),
      }),
      transformResponse: parseAuthToken,
    }),
  }),
})

export const { useExchangeCodeMutation, useRefreshAccessMutation } = spotifyAccountsApiSlice
