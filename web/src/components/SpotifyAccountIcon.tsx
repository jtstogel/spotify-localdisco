import './SpotifyAccountIcon.css'
import { useAppSelector } from '../app/hooks'
import { useGetMyProfileQuery } from '../features/spotify/spotifyApiSlice';
import type { SpotifyAuthToken } from '../features/spotify/spotifyAccountsApiSlice';
import { selectAuthTokens } from '../features/spotify/spotifySlice';

const SpotifyAccountIcon = () => {
    const authTokens = useAppSelector(selectAuthTokens);
    if (!authTokens) {
        return <></>
    }
    return <LoggedInSpotifyIcon authTokens={authTokens} />
};

const LoggedInSpotifyIcon = ({ authTokens }: { authTokens: SpotifyAuthToken }) => {
    const { data: profile, isLoading, error } = useGetMyProfileQuery(authTokens);
    if (!authTokens) {
        return (<div>not logged in</div>)
    }

    if (isLoading) { return (<></>) }
    if (error) { return (<div>{String(error)}</div>) }
    const imageUrl = profile?.images?.[0]?.url;

    return (
        <div>
            <img className="profile-image" alt="spotify profile" src={imageUrl}></img>
        </div>
    )
};

export default SpotifyAccountIcon;
