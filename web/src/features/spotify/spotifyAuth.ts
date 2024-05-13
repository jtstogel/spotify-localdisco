import type { AppStore } from "../../app/store";
import { promiseResolver } from "../api/api";
import { exchangeAuthToken, getClientId, getUserProfile } from "./spotifyApi"
import { LocalStorageKey, getItem, setItem } from '../../utils/storage';
import {
    clientIdLoaded,
    selectClientId,
    authChange,
    selectOAuth2State,
    SpotifyUserProfile,
    selectUserProfile,
    isTerminalAuthState,
    userProfileLoaded,
    setLocalStorageAuth,
} from "./spotifySlice"

export const SPOTIFY_CODE_REDIRECT_PATH = "/spotify/auth"

function codeRedirectUri() {
    return window.location.origin + SPOTIFY_CODE_REDIRECT_PATH
}

async function fetchClientId(store: AppStore) {
    let clientId = selectClientId(store.getState())
    if (clientId) {
        return clientId
    }

    clientId = await getClientId()
    store.dispatch(clientIdLoaded(clientId))
    return clientId
}

function isBefore(timeString: string): boolean {
    return Date.parse(timeString) < Date.now();
}

async function fetchProfileDetails(accessToken: string, store: AppStore): Promise<SpotifyUserProfile | null> {
    let userProfile = selectUserProfile(store.getState())
    if (userProfile) {
        return userProfile
    }

    userProfile = await getUserProfile(accessToken);
    store.dispatch(userProfileLoaded(userProfile))
    return userProfile
}

export function generateRandomString(length: number) {
    const possible =
        "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"
    const values = crypto.getRandomValues(new Uint8Array(length))
    return values.reduce((acc, x) => acc + possible[x % possible.length], "")
}

function spotifyOAuth2Redirect({
    clientId,
    state,
    width,
    height,
    redirectUri,
}: {
    state: string
    clientId: string
    redirectUri: string
    width: number
    height: number
}) {
    const scope =
        "playlist-modify-private user-top-read user-library-read ugc-image-upload"
    const url = `https://accounts.spotify.com/authorize?${new URLSearchParams([
        ["response_type", "code"],
        ["client_id", clientId],
        ["state", state],
        ["scope", scope],
        ["redirect_uri", redirectUri],
        ["show_dialog", "true"],
    ]).toString()}`
    return window.location.href = url;
}

export async function loginViaOAuth2(store: AppStore) {
    const state = generateRandomString(16)
    const redirectUri = codeRedirectUri()
    const clientId = await fetchClientId(store)

    openOAuth2LoginPopup({
        state,
        clientId,
        redirectUri,
        width: 700,
        height: 900,
    });
    store.dispatch(
        authChange({
            stage: "userRedirected",
            state,
            redirectUri,
            openTime: new Date().toISOString(),
        }),
    )

    const unsubscribe = store.subscribe(async () => {
        const oAuthState = selectOAuth2State(store.getState())
        const pendingOrDone =
            oAuthState?.stage === "codeReceived" ||
            oAuthState?.stage === "userRedirected"
        if (!pendingOrDone || oAuthState.state !== state) {
            return unsubscribe()
        }

        if (oAuthState.stage !== "codeReceived") {
            return // Still waiting...
        }

        const { code, redirectUri } = oAuthState
        const start = new Date()
        store.dispatch(
            authChange({
                stage: "codeExhangePending",
                startTime: start.toISOString(),
            }),
        )

        try {
            const { accessToken, expiresIn, refreshToken } = await exchangeAuthToken(
                code,
                redirectUri,
            )
            // Make our expireTime artificially sooner to account for request time.
            const expireTime = new Date(
                (expiresIn - 10) * 1000 + start.getTime(),
            ).toISOString()
            const oAuthState = {
                stage: "complete" as const,
                tokens: { accessToken, refreshToken, expireTime },
            };
            store.dispatch(authChange(oAuthState))
            setLocalStorageAuth(oAuthState);
            fetchProfileDetails(accessToken, store);
        } catch (error: unknown) {
            console.log(error);
            store.dispatch(authChange({ stage: "error", error: String(error) }))
        }

        return unsubscribe()
    })
}

async function getAuth(store: AppStore): Promise<string | null> {
    let authState = selectOAuth2State(store.getState());
    if (authState?.stage === 'complete') {
        const { tokens } = authState;
        if (isBefore(tokens.expireTime)) {
            return tokens.accessToken;
        }

        const startTime = new Date().toISOString();
        authState = { stage: 'refreshPending', tokens, startTime };
        store.dispatch(authChange(authState));
    }

    if (!authState || !isTerminalAuthState(authState)) {
        return null;
    }

    const resolver = promiseResolver<string | null>();
    const unsubscribe = store.subscribe(() => {
        const authState = selectOAuth2State(store.getState());
        if (authState && !isTerminalAuthState(authState)) {
            return;
        }
        if (authState?.stage === 'complete') {
            return authState.tokens.accessToken;
        }
        return null;
    });

    return resolver.promise.finally(() => {
        unsubscribe();
    });
}

interface SpotifyOAuth2CodeResponse {
    code: string
    state: string
    redirectUri: string
    receiveTime: string
}

function getOAuth2ResponseCode(w: Window): SpotifyOAuth2CodeResponse | null {
    return getItem<SpotifyOAuth2CodeResponse>(w, LocalStorageKey.CODE);
}

export function setOAuth2ResponseCode(
    code: SpotifyOAuth2CodeResponse,
    w: Window,
) {
    return setItem(w, LocalStorageKey.CODE, code);
}

export const localStorageListener =
    (store: AppStore, w: Window) => (event: StorageEvent) => {
        if (event.key !== LocalStorageKey.CODE) return
        const response = getOAuth2ResponseCode(w)
        if (!response) {
            return
        }

        const authState = selectOAuth2State(store.getState())
        if (
            authState?.stage !== "popupOpened" ||
            authState.state !== response.state
        ) {
            return
        }

        store.dispatch(authChange({ stage: "codeReceived", ...response }))
    }
