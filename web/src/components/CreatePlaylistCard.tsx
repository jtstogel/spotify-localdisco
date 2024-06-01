import styles from "./CreatePlaylistCard.module.css"
import { useCreatePlaylistJobMutation } from "../features/api/apiSlice"
import type { PlaylistConfig } from "../components/PlaylistConfigurationForm";
import PlaylistConfigurationForm from "../components/PlaylistConfigurationForm"
import CreatePlaylistStatus from './CreatePlaylistStatus'
import '../styles/spinner.css'
import { FetchBaseQueryError } from "@reduxjs/toolkit/query";

interface AppError {
    error?: string;
}

function isAppError(err: unknown): err is AppError {
    if (!err) return false;

    const appError = err as AppError;
    return Boolean(appError.error && typeof appError.error === 'string');
}

const CreatePlaylistCard = ({ authToken }: { authToken: string }) => {
    const [createPlaylistJob, { data: response, error, isLoading }] = useCreatePlaylistJobMutation()

    const onSubmit = (config: PlaylistConfig) => {
        console.log(config);
        createPlaylistJob({
            ...config,
            authToken,
        })
    }

    if (isLoading) {
        return (
            <div className={styles.container}>
                <span className={'spinner ' + styles['spinner']}></span>
            </div>
        )
    }
    const errorData = (error as FetchBaseQueryError|undefined)?.data
    if (isAppError(errorData)) {
        return (
            <div className={styles.container}>
                <div className={styles['error']}>Error: {errorData.error}</div>
            </div>
        )
    }

    if (response) {
        return (
            <div className={styles.container}>
                <CreatePlaylistStatus name={response.name} />
            </div>
        )
    }

    return (
        <div className={styles.container}>
            <PlaylistConfigurationForm onSubmit={onSubmit} />
        </div>
    )
}

export default CreatePlaylistCard
