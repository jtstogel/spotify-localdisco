import './SpotifyAccountIcon.css'
import { useAppSelector } from '../app/hooks'
import { useGetUserProfileQuery } from '../features/spotify/spotifyApiSlice';
import { selectAuthTokens, SpotifyUserAuthTokens } from '../features/spotify/spotifySlice';

const SpotifyAccountIcon = () => {
    const authTokens = useAppSelector(selectAuthTokens);
    if (!authTokens) {
        return <></>
    }
    return <LoggedInSpotifyIcon authTokens={authTokens} />
};

const LoggedInSpotifyIcon = ({ authTokens }: { authTokens: SpotifyUserAuthTokens }) => {
    const { data: profile, isLoading, error } = useGetUserProfileQuery({ accessToken: authTokens.accessToken });

    if (!authTokens) {
        return (<div>not logged in</div>)
    }

    if (isLoading) return (<></>)
    if (error) return (<div>{String(error)}</div>)
    return (
        <div>
            <img className="profile-image" src={profile!.profileImageUrl}></img>
        </div>
    )
};

export default SpotifyAccountIcon;
