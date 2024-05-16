import "./Home.css"
import { useEffect } from "react"
import { useNavigate } from "react-router-dom"
import { selectUserAuthenticated } from "../features/spotify/spotifySlice"
import { useAppSelector } from "../app/hooks"

const Home = () => {
  const isAuthenticatedWithSpotify = useAppSelector(selectUserAuthenticated);
  const navigate = useNavigate();
  useEffect(() => {
    if (!isAuthenticatedWithSpotify) {
      navigate('/spotify/login')
    }
  }, [isAuthenticatedWithSpotify, navigate])

  return (<div className="App"></div>)
}

export default Home
