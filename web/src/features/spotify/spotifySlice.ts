import { createAppSlice } from "../../app/createAppSlice"
import type { SpotifyOAuthParams } from "./spotifyOAuth"
import { createAsyncThunk } from '@reduxjs/toolkit'
import { apiSlice } from "../api/apiSlice";

export interface SpotifyUserProfile {
    displayName: string
    profileImageUrl?: string
}

export interface SpotifyState {
    clientId?: string
    oAuthParams?: SpotifyOAuthParams;
    discoUserId?: string
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
        oAuthFlowInitiated: create.reducer<SpotifyOAuthParams>((state, { payload }) => {
            state.oAuthParams = payload
        }),
        codeExhanged: create.reducer<string>((state, { payload }) => {
            state.exhangedCodes ??= []
            state.exhangedCodes.push(payload)
        }),
    }),
    extraReducers: (builder) => {
        builder.addMatcher(
            apiSlice.endpoints.authenticateWithSpotify.matchFulfilled,
            (state, { payload }) => {
                state.discoUserId = payload.userId
            },
        );
        builder.addMatcher(
            apiSlice.endpoints.getSpotifyProfile.matchRejected,
            (state, err) => {
                if (err.payload?.status === 403) {
                    state.discoUserId = undefined;
                }
            },
        );
    },
    selectors: {
        selectClientId: state => state.clientId,
        selectAuthToken: state => state.discoUserId,
        selectUserAuthenticated: state => !!state.discoUserId,
        selectUserProfile: state => state.userProfile,
        selectHasExhangedCode: (state, code: string) => !!state.exhangedCodes?.includes(code),
        selectOAuthParams: state => state.oAuthParams
    },
})

export const {
    clientIdLoaded,
    userProfileLoaded,
    oAuthFlowInitiated,
} = spotifySlice.actions

export const {
    selectClientId,
    selectAuthToken,
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

        thunkAPI.dispatch(apiSlice.endpoints.authenticateWithSpotify.initiate({
            code,
            redirectUri: oAuthParams.redirectUri
        }));
    },
)
