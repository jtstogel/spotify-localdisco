import styles from './SpotifyButton.module.css';
import spotifyIconImg from './spotify-icon.png'
import { MouseEventHandler, ReactNode } from "react";

const SpotifyButton = (
    { onClick, children, backgroundColor }: {
        onClick: MouseEventHandler<HTMLButtonElement>,
        children: ReactNode,
        backgroundColor?: string,
    },
) => {
    return (
        <button className={styles['button']} style={{backgroundColor}} onClick={onClick}>
            <span>{children}</span>
            <img className={styles['spotify-icon']} src={spotifyIconImg}></img>
        </button>
    )
};

export default SpotifyButton
