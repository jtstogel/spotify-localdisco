import styles from "./CreatePlaylistStatus.module.css"
import { useState } from "react"
import { useGetPlaylistJobQuery } from "../features/api/apiSlice"
import SpotifyButton from "./SpotifyButton";

const CreatePlaylistStatus = ({ name }: { name: string }) => {
  const [poll, setPoll] = useState(true);
  const [statuses, setStatuses] = useState<string[]>([]);

  const { data, error } = useGetPlaylistJobQuery({ name }, {
    pollingInterval: poll ? 1000 : 0,
  });

  const message = data?.metadata?.message;
  if (message && message !== statuses[statuses.length - 1]) {
    setStatuses(statuses.concat([message]))
  }

  if (!data?.done) {
    const succeededStatuses = statuses.slice(0, statuses.length - 1);
    const pendingStatus = statuses[statuses.length - 1];

    return <div className={styles.container}>
      <h4>Curating your playlist...</h4>
      <ul>
        {succeededStatuses.map(m => <li>{m} &#10003;</li>)}
        {pendingStatus && <li>{loadingText(pendingStatus)}</li>}
      </ul>
    </div>
  }

  if (poll) {
    setPoll(false);
  }

  if (error || data?.error) {
    return <div>
      Failed to get playlist status... Please try again later!
      <div>{JSON.stringify(error || data?.error)}</div>
    </div>
  }

  const openInSpotify = () => {
    const url = data.result?.playlistLink!;
    window.open(url, '_blank');
  };

  const artists = data.result?.artists ?? [];

  if (artists.length === 0) {
    return <div className={styles.container}>
      <h4>No events found! 😕</h4>
    </div>
  }

  return <div className={styles.container}>
    <SpotifyButton onClick={openInSpotify} backgroundColor="#fff">Open spotify</SpotifyButton>
    <h4>Your playlist contains artists like...</h4>
    <ul>
      {artists.slice(0, 10).map(a => <li>{a}</li>)}
      {artists.length > 10 && <li>And {artists.length - 10} more...</li>}
    </ul>
  </div>
}

function loadingText(text: string) {
  const words = text.split(' ');
  const lastWord = words.pop();
  const lastWordHtml = <span className={styles.loading}>{lastWord}</span>;
  return !words.length ? lastWordHtml : <span>{words} {lastWordHtml}</span>;
}

export default CreatePlaylistStatus
