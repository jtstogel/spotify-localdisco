export const SPOTIFY_OAUTH_REDIRECT_PATH = "/spotify/auth"

const SPOTIFY_OAUTH_SCOPE = "playlist-modify-private user-follow-read user-top-read user-library-read";

function oAuthRedirectUri() {
    return window.location.origin + SPOTIFY_OAUTH_REDIRECT_PATH
}

export declare interface SpotifyOAuthParams {
    redirectUri: string;
    clientId: string;
}

export async function generateOAuthParams(clientId: string): Promise<SpotifyOAuthParams> {
    return {
        clientId,
        redirectUri: oAuthRedirectUri(),
    }
};

export async function redirectToSpotifyOAuth2(args: SpotifyOAuthParams) {
    const redirectUri = oAuthRedirectUri()
    const authUrl = new URL("https://accounts.spotify.com/authorize")
    authUrl.search = new URLSearchParams([
        ["response_type", "code"],
        ["client_id", args.clientId],
        ["scope", SPOTIFY_OAUTH_SCOPE],
        ["redirect_uri", redirectUri],
    ]).toString()
    window.location.href = authUrl.toString()
}
