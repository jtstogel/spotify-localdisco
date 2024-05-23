import { useGetSpotifyClientIdQuery } from "../features/api/apiSlice";
import { generateOAuthParams, redirectToSpotifyOAuth2 } from "../features/spotify/spotifyOAuth";
import { useAppDispatch } from "../app/hooks";
import { oAuthFlowInitiated } from "../features/spotify/spotifySlice";
import { saveStore } from "../app/store";
import './SpotifyLogin.css';

const SpotifyLogin = () => {
    const { data: clientId, isLoading, error } = useGetSpotifyClientIdQuery();
    const dispatch = useAppDispatch();

    if (isLoading) { return <></> }
    if (error) { return <div>client load failed: {JSON.stringify(error)}</div> }

    const login = async () => {
        const oAuthParams = await generateOAuthParams(clientId!)
        dispatch(oAuthFlowInitiated(oAuthParams))
        // Ensure the store is saved before redirecting the user.
        saveStore();
        redirectToSpotifyOAuth2(oAuthParams);
    }

    return (
        <div>
            <button className="login-button" onClick={login}>Login with Spotify</button>
        </div>
    )
};

export default SpotifyLogin
