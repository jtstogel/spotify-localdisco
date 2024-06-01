import styles from "./CreatePlaylistCard.module.css"
import { useCreatePlaylistJobMutation } from "../features/api/apiSlice"
import type { PlaylistConfig } from "../components/PlaylistConfigurationForm";
import PlaylistConfigurationForm from "../components/PlaylistConfigurationForm"
import CreatePlaylistStatus from './CreatePlaylistStatus'

const CreatePlaylistCard = ({authToken}: {authToken: string}) => {
    const [createPlaylistJob, { data: response }] = useCreatePlaylistJobMutation()

    const onSubmit = (config: PlaylistConfig) => {
        createPlaylistJob({
            ...config,
            authToken,
        })
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
