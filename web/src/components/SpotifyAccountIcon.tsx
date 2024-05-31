import './SpotifyAccountIcon.css'
import { useAppDispatch, useAppSelector } from '../app/hooks'
import { selectAuthToken, signOutInitiated } from '../features/spotify/spotifySlice';
import { useGetSpotifyProfileQuery } from '../features/api/apiSlice';
import { MouseEventHandler, useState, useEffect } from 'react';

const SpotifyAccountIcon = () => {
    const authToken = useAppSelector(selectAuthToken);
    if (!authToken) {
        return <></>
    }
    return <LoggedInSpotifyIcon token={authToken} />
};

const LoggedInSpotifyIcon = ({ token }: { token: string }) => {
    const { data: profile, isLoading, error } = useGetSpotifyProfileQuery(token);
    const dispatch = useAppDispatch();
    const [optionsOpen, setOptionsOpen] = useState(false);

    useEffect(() => {
        const closeOptions = () => {
            if (optionsOpen) {
                setOptionsOpen(false);
            }
        };

        document.body.addEventListener('click', closeOptions);
        return () => {
            document.body.removeEventListener('click', closeOptions);
        };
    });
    
    if (!token) {
        return (<div>not logged in</div>)
    }

    if (isLoading) {
        return (<></>)
    }

    if (error) {
        return (<div>authentication failed!</div>)
    }

    const toggleOptionsOpen: MouseEventHandler<HTMLImageElement> = event => {
        event.preventDefault();
        event.stopPropagation();
        setOptionsOpen(!optionsOpen);
    };

    const signOut: MouseEventHandler<HTMLButtonElement> = event => {
        event.preventDefault();
        event.stopPropagation();
        dispatch(signOutInitiated());
    };

    return (
        <div>
            <img className="profile-image" alt="spotify profile" onClick={toggleOptionsOpen} src={profile?.profileImageUrl} />
            <div className={'options-dropdown ' + (optionsOpen ? 'open' : 'closed')}>
                <button className="option" onClick={signOut}>Sign out</button>
            </div>
        </div>
    )
};

export default SpotifyAccountIcon;
