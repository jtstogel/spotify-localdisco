import './SpotifyAccountIcon.css'
import { useAppSelector } from '../app/hooks'
import { selectAuthToken } from '../features/spotify/spotifySlice';
import { useGetSpotifyProfileQuery } from '../features/api/apiSlice';

const SpotifyAccountIcon = () => {
    const authToken = useAppSelector(selectAuthToken);
    if (!authToken) {
        return <></>
    }
    return <LoggedInSpotifyIcon token={authToken} />
};

const LoggedInSpotifyIcon = ({ token }: { token: string }) => {
    const { data: profile, isLoading, error } = useGetSpotifyProfileQuery(token);
    if (!token) {
        return (<div>not logged in</div>)
    }

    if (isLoading) { return (<></>) }
    if (error) {
        return (<div>authentication failed!</div>)
    }

    return (
        <div>
            <img className="profile-image" alt="spotify profile" src={profile?.profileImageUrl}></img>
        </div>
    )
};

export default SpotifyAccountIcon;
