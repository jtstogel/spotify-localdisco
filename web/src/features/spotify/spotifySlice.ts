import { createAppSlice } from "../../app/createAppSlice"
import { store } from "../../app/store";
import { LocalStorageKey, getItem, setItem } from '../../utils/storage';

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

interface OAuth2Uninitalized {
    stage: "uninitialized"
}

interface OAuth2UserRedirected {
    stage: "userRedirected"
    state: string
    redirectUri: string
    openTime: string
}

interface OAuth2CodeReceived {
    stage: "codeReceived"
    code: string
    state: string
    redirectUri: string
    receiveTime: string
}

interface OAuth2CodeExhangePending {
    stage: "codeExhangePending"
    startTime: string
}

interface OAuth2CodeRefreshPending {
    stage: "refreshPending"
    startTime: string
    tokens: SpotifyUserAuthTokens
}

interface OAuth2Complete {
    stage: "complete"
    tokens: SpotifyUserAuthTokens
}

interface OAuth2Error {
    stage: "error"
    error: unknown
}

type OAuth2State =
    | OAuth2Uninitalized // -> OAuth2UserRedirected
    | OAuth2Error // -> OAuth2Uninitalized
    | OAuth2UserRedirected // -> OAuth2CodeReceived, OAuth2Error
    | OAuth2CodeReceived // -> OAuth2CodeExhangePending, OAuth2Error
    | OAuth2CodeExhangePending // -> OAuth2Complete, OAuth2Error
    | OAuth2CodeRefreshPending // -> OAuth2Complete, OAuth2Error
    | OAuth2Complete // -> OAuth2CodeRefreshPending

export function isTerminalAuthState(authState: OAuth2State) {
    const { stage } = authState;
    return stage === 'error' || stage === 'complete' || stage === 'uninitialized';
}

export interface SpotifyState {
    clientId?: string
    auth?: OAuth2State
    userProfile?: SpotifyUserProfile
}

export function setLocalStorageAuth(auth: OAuth2State) {
    if (auth.stage === 'complete') {
        setItem(window, LocalStorageKey.TOKENS, auth);
    }
}

function getLocalStorageAuth(): OAuth2State | undefined {
    return getItem<OAuth2State>(window, LocalStorageKey.TOKENS) ?? undefined;
}

const initialState: SpotifyState = {
    auth: getLocalStorageAuth(),
}

// If you are not using async thunks you can use the standalone `createSlice`.
export const spotifySlice = createAppSlice({
    name: "spotify",
    // `createSlice` will infer the state type from the `initialState` argument
    initialState,
    // The `reducers` field lets us define reducers and generate associated actions
    reducers: create => ({
        clientIdLoaded: create.reducer<string>((state, { payload }) => {
            state.clientId = payload
        }),
        userProfileLoaded: create.reducer<SpotifyUserProfile>((state, { payload }) => {
            state.userProfile = payload
        }),
        authChange: create.reducer<OAuth2State>((state, { payload }) => {
            state.auth = payload
        }),
    }),
    // You can define your selectors here. These selectors receive the slice
    // state as their first argument.
    selectors: {
        selectClientId: state => state.clientId,
        selectOAuth2State: state => state.auth,
        selectUserProfile: state => state.userProfile,
        selectUserAccessToken: state => state.auth?.stage === 'complete' ? state.auth.tokens.accessToken : null,
    },
})

// Action creators are generated for each case reducer function.
export const { clientIdLoaded, authChange, userProfileLoaded } = spotifySlice.actions

// Selectors returned by `slice.selectors` take the root state as their first argument.
export const { selectClientId, selectOAuth2State, selectUserProfile } = spotifySlice.selectors
