export const SPOTIFY_CODE_REDIRECT_PATH = "/spotify/auth"

function codeRedirectUri() {
    return window.location.origin + SPOTIFY_CODE_REDIRECT_PATH
}

export async function redirectToSpotifyOAuth2(clientId: string) {
    const redirectUri = codeRedirectUri()
    const scope =
        "playlist-modify-private user-top-read user-library-read ugc-image-upload"
    window.location.href = `https://accounts.spotify.com/authorize?${new URLSearchParams([
        ["response_type", "code"],
        ["client_id", clientId],
        ["scope", scope],
        ["redirect_uri", redirectUri],
        ["show_dialog", "true"],
    ]).toString()}`
}
