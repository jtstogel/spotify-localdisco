import { createAppSlice } from "../../app/createAppSlice"
import { spotifyApiSlice } from "./spotifyApiSlice"

export interface SpotifyUserAuthTokens {
    accessToken: string

    // Token for retrieving the next token after expiration.
    refreshToken: string

    // Expiration time for the token in RFC3339 format.
    expireTime: string
}

export interface SpotifyUserProfile {
    displayName: string
    profileImageUrl?: string
}

export interface SpotifyState {
    clientId?: string
    authTokens?: SpotifyUserAuthTokens
    userProfile?: SpotifyUserProfile
}

interface OAuth2CodeResponse {
    code: string;
    redirectUri: string;
}

const initialState: SpotifyState = {}

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
        authTokensLoaded: create.reducer<SpotifyUserAuthTokens>((state, { payload }) => {
            state.authTokens = payload
        }),
        authCodeReceived: create.asyncThunk(
            (response: OAuth2CodeResponse, thunkAPI) => {
                return thunkAPI.dispatch(spotifyApiSlice.endpoints.exchangeCode.initiate(response))
            }
        ),
    }),
    extraReducers: (builder) => {
        builder.addMatcher(
            spotifyApiSlice.endpoints.exchangeCode.matchFulfilled,
            (state, { payload: response } ) => {
                const expireTime = new Date(response.expiresIn * 1000 - 10 + Date.now()).toString()
                state.authTokens = {
                    accessToken: response.accessToken,
                    refreshToken: response.refreshToken,
                    expireTime,
                }
            },
        );
    },
    selectors: {
        selectClientId: state => state.clientId,
        selectAuthTokens: state => state.authTokens,
        selectUserProfile: state => state.userProfile,
        selectUserAuthenticated: state => !!state.authTokens
    },
})

// Action creators are generated for each case reducer function.
export const { clientIdLoaded, authTokensLoaded, userProfileLoaded, authCodeReceived } = spotifySlice.actions

// Selectors returned by `slice.selectors` take the root state as their first argument.
export const { selectClientId, selectAuthTokens, selectUserProfile, selectUserAuthenticated } = spotifySlice.selectors
