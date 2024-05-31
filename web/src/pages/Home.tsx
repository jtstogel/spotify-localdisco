import "./Home.css"
import { useEffect } from "react"
import { useNavigate } from "react-router-dom"
import { selectAuthToken } from "../features/spotify/spotifySlice"
import { useAppSelector } from "../app/hooks"
import { useCreatePlaylistJobMutation } from "../features/api/apiSlice"
import PlaylistConfigurationForm, { PlaylistConfig } from "../components/PlaylistConfigurationForm"
import CreatePlaylistStatusCard from '../components/CreatePlaylistStatus'
import CreatePlaylistCard from "../components/CreatePlaylistCard"

const Home = () => {
  const authToken = useAppSelector(selectAuthToken);
  const [createPlaylistJob, { data: response }] = useCreatePlaylistJobMutation()
  const navigate = useNavigate();

  useEffect(() => {
    if (!authToken) {
      navigate('/spotify/login');
    }
  }, [authToken, navigate]);

  if (!authToken) {
    return <></>
  }
  const onSubmit = (config: PlaylistConfig) => {
    createPlaylistJob({
      ...config,
      authToken,
    })
  }

  if (response) {
    return <div className="App"><CreatePlaylistStatusCard name={response.name}></CreatePlaylistStatusCard></div>
  }

  return (
    <div className="App"><CreatePlaylistCard authToken={authToken}/></div>
  )
}

export default Home
