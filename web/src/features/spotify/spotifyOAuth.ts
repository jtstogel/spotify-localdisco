export const SPOTIFY_OAUTH_REDIRECT_PATH = "/spotify/auth"

const SPOTIFY_OAUTH_SCOPE = "playlist-modify-private user-follow-read user-top-read user-library-read ugc-image-upload";

function oAuthRedirectUri() {
    return window.location.origin + SPOTIFY_OAUTH_REDIRECT_PATH
}

export declare interface PKCEChallenge {
    codeVerifier: string;
    codeChallenge: string;
}

export declare interface SpotifyOAuthParams {
    pkceChallenge: PKCEChallenge;
    redirectUri: string;
    clientId: string;
}

const generateRandomString = (length: number) => {
    const possible = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789';
    const values = crypto.getRandomValues(new Uint8Array(length));
    return values.reduce((acc, x) => acc + possible[x % possible.length], "");
}

const sha256 = async (plainText: string) => {
    const encoder = new TextEncoder()
    const data = encoder.encode(plainText)
    return window.crypto.subtle.digest('SHA-256', data)
}

const urlSafeBase64Encode = (input: ArrayBuffer) => {
    return btoa(String.fromCharCode(...new Uint8Array(input)))
        .replace(/=/g, '')
        .replace(/\+/g, '-')
        .replace(/\//g, '_');
}

const generatePKCEChallenge: () => Promise<PKCEChallenge> = async () => {
    const codeVerifier = generateRandomString(64);
    const codeChallenge = urlSafeBase64Encode(await sha256(codeVerifier));
    return { codeVerifier, codeChallenge };
};

export async function generateOAuthParams(clientId: string): Promise<SpotifyOAuthParams> {
    return {
        clientId,
        pkceChallenge: await generatePKCEChallenge(),
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
        ["code_challenge", args.pkceChallenge.codeChallenge],
        ["code_challenge_method", 'S256'],
        ["show_dialog", "true"],
    ]).toString()
    window.location.href = authUrl.toString()
}
