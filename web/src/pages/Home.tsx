import "./Home.css"
import { useEffect } from "react"
import { useNavigate } from "react-router-dom"
import { selectAuthToken } from "../features/spotify/spotifySlice"
import { useAppSelector } from "../app/hooks"
import CreatePlaylistCard from "../components/CreatePlaylistCard"

const Home = () => {
  const authToken = useAppSelector(selectAuthToken);
  const navigate = useNavigate();

  useEffect(() => {
    if (!authToken) {
      navigate('/spotify/login');
    }
  }, [authToken, navigate]);

  if (!authToken) {
    return <></>
  }

  return (
    <div className="App"><CreatePlaylistCard authToken={authToken}/></div>
  )
}

export default Home
