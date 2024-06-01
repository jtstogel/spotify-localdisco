import "./Home.css"
import { selectAuthToken } from "../features/spotify/spotifySlice"
import { useAppSelector } from "../app/hooks"
import CreatePlaylistCard from "../components/CreatePlaylistCard"
import SpotifyLogin from "./SpotifyLogin"

const Home = () => {
  const authToken = useAppSelector(selectAuthToken);
  if (!authToken) {
    return <SpotifyLogin />
  }
  return (
    <div className="App"><CreatePlaylistCard authToken={authToken}/></div>
  )
}

export default Home
