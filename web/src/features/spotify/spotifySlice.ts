import { GetThunkAPI } from "@reduxjs/toolkit";
import { createAppSlice } from "../../app/createAppSlice"
import type { SpotifyAuthToken } from "./spotifyAccountsApiSlice";
import { spotifyAccountsApiSlice } from "./spotifyAccountsApiSlice"
import type { SpotifyOAuthParams } from "./spotifyOAuth"
import { createAsyncThunk } from '@reduxjs/toolkit'

export interface SpotifyUserProfile {
    displayName: string
    profileImageUrl?: string
}

export interface SpotifyState {
    clientId?: string
    oAuthParams?: SpotifyOAuthParams;
    authTokens?: SpotifyAuthToken
    userProfile?: SpotifyUserProfile
    exhangedCodes?: string[];
}

const initialState: SpotifyState = {}

type RootState = { spotify: SpotifyState };

export const spotifySlice = createAppSlice({
    name: "spotify",
    initialState,
    reducers: create => ({
        clientIdLoaded: create.reducer<string>((state, { payload }) => {
            state.clientId = payload
        }),
        userProfileLoaded: create.reducer<SpotifyUserProfile>((state, { payload }) => {
            state.userProfile = payload
        }),
        authTokensLoaded: create.reducer<SpotifyAuthToken>((state, { payload }) => {
            state.authTokens = payload
        }),
        oAuthFlowInitiated: create.reducer<SpotifyOAuthParams>((state, { payload }) => {
            state.oAuthParams = payload
        }),
        codeExhanged: create.reducer<string>((state, { payload }) => {
            state.exhangedCodes ??= []
            state.exhangedCodes.push(payload)
        }),
        authCodeReceived: create.asyncThunk(
            (code: string, thunkAPI) => {
                const state = (thunkAPI.getState() as RootState);
                const oAuthParams = state.spotify.oAuthParams;
                if (!oAuthParams) {
                    throw new Error('authenticate not requested')
                }
                thunkAPI.dispatch(spotifyAccountsApiSlice.endpoints.exchangeCode.initiate({
                    code,
                    clientId: oAuthParams.clientId,
                    redirectUri: oAuthParams.redirectUri,
                    codeVerifier: oAuthParams.pkceChallenge.codeVerifier,
                }))
            }
        ),
    }),
    extraReducers: (builder) => {
        builder.addMatcher(
            spotifyAccountsApiSlice.endpoints.exchangeCode.matchFulfilled,
            (state, { payload: authTokens }) => {
                state.authTokens = authTokens
            },
        );
    },
    selectors: {
        selectClientId: state => state.clientId,
        selectAuthTokens: state => state.authTokens,
        selectUserAuthenticated: state => !!state.authTokens,
        selectUserProfile: state => state.userProfile,
        selectHasExhangedCode: (state, code: string) => !!state.exhangedCodes?.includes(code),
        selectOAuthParams: state => state.oAuthParams
    },
})

export const {
    clientIdLoaded,
    authTokensLoaded,
    userProfileLoaded,
    oAuthFlowInitiated,
} = spotifySlice.actions

export const {
    selectClientId,
    selectAuthTokens,
    selectUserProfile,
    selectUserAuthenticated,
    selectHasExhangedCode
} = spotifySlice.selectors

export const authCodeReceived = createAsyncThunk(
    'spotify/authCodeReceived',
    async (code: string, thunkAPI) => {
        const state = (thunkAPI.getState() as RootState);
        const oAuthParams = state.spotify.oAuthParams;
        if (!oAuthParams) {
            throw new Error('authenticate not requested')
        }

        // Prevent duplicate exhanges.
        if (selectHasExhangedCode(state, code)) { return; }
        thunkAPI.dispatch(spotifySlice.actions.codeExhanged(code));

        thunkAPI.dispatch(spotifyAccountsApiSlice.endpoints.exchangeCode.initiate({
            code,
            clientId: oAuthParams.clientId,
            redirectUri: oAuthParams.redirectUri,
            codeVerifier: oAuthParams.pkceChallenge.codeVerifier,
        }))
    },
)
