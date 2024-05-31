import styles from './SpotifyAccountIcon.module.css'
import { useAppDispatch, useAppSelector } from '../app/hooks'
import { selectAuthToken, signOutInitiated } from '../features/spotify/spotifySlice';
import { useGetSpotifyProfileQuery } from '../features/api/apiSlice';
import { MouseEventHandler, useState, useEffect } from 'react';

const SpotifyAccountIcon = () => {
    const authToken = useAppSelector(selectAuthToken);
    if (!authToken) {
        return <></>
    }
    return <LoggedInSpotifyIcon authToken={authToken} />
};

const LoggedInSpotifyIcon = ({ authToken }: { authToken: string }) => {
    const { data: profile, isLoading, error } = useGetSpotifyProfileQuery(authToken);
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
            <img className={styles['profile-image']} alt="spotify profile" onClick={toggleOptionsOpen} src={profile?.profileImageUrl} />
            <div className={styles['options-dropdown'] + ' ' + (optionsOpen ? styles['open'] : styles['closed'])}>
                <button className={styles['option']} onClick={signOut}>Sign out</button>
            </div>
        </div>
    )
};

export default SpotifyAccountIcon;
