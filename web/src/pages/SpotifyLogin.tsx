import { useGetSpotifyClientIdQuery } from "../features/api/apiSlice";
import { generateOAuthParams, redirectToSpotifyOAuth2 } from "../features/spotify/spotifyOAuth";
import { useAppDispatch, useAppSelector } from "../app/hooks";
import { oAuthFlowInitiated, selectUserAuthenticated } from "../features/spotify/spotifySlice";
import { saveStore } from "../app/store";
import './SpotifyLogin.css';
import spotifyIconImg from './spotify-icon.png'
import { useNavigate } from "react-router-dom";
import { useEffect } from "react";

const SpotifyLogin = () => {
    const { data: clientId, isLoading, error } = useGetSpotifyClientIdQuery();
    const dispatch = useAppDispatch();
    const isAuthenticated = useAppSelector(selectUserAuthenticated);
    const navigate = useNavigate();

    useEffect(() => {
        if (isAuthenticated) {
            navigate('/');
        }
    }, [navigate, isAuthenticated]);

    if (isLoading) {
        return <></>
    }
    if (error) {
        return <div>client load failed: {JSON.stringify(error)}</div>
    }

    const login = async () => {
        const oAuthParams = await generateOAuthParams(clientId!)
        dispatch(oAuthFlowInitiated(oAuthParams))
        // Ensure the store is saved before redirecting the user.
        saveStore();
        redirectToSpotifyOAuth2(oAuthParams);
    }

    return (
        <div>
            <div className="description">
                <p>Log in to make a playlist of artists playing nearby.</p>
            </div>
            <button className="login-button" onClick={login}>
                Log in with Spotify
                <img className="spotify-icon" src={spotifyIconImg}></img>
            </button>
        </div>
    )
};

export default SpotifyLogin
