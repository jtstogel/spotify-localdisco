import { useGetClientIdQuery } from "../features/spotify/spotifyApiSlice";
import { redirectToSpotifyOAuth2 } from "../features/spotify/spotifyAuth";

const SpotifyLogin = () => {
    const { data: clientId, isLoading } = useGetClientIdQuery();
    if (isLoading) return (<></>)

    return (
        <div>
            <button onClick={() => redirectToSpotifyOAuth2(clientId!)}>Login with Spotify</button>
        </div>
    )
};

export default SpotifyLogin
