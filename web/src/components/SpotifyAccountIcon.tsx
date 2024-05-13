import { useAppSelector } from '../app/hooks'
import { selectUserProfile } from '../features/spotify/spotifySlice';
import { loginViaOAuth2 } from '../features/spotify/spotifyAuth';
import { store } from '../app/store';

const SpotifyAccountIcon = () => {
    const userProfile = useAppSelector(selectUserProfile);
    if (userProfile) {
        return (
            <div>
                <img src={userProfile.profileImageUrl}></img>
            </div>
        )
    }
    return (
        <div>
            <button onClick={() => loginViaOAuth2(store)}>Login with Spotify</button>
        </div>
    )
};

export default SpotifyAccountIcon;
