import { useSearchParams } from "react-router-dom"
import { setOAuth2ResponseCode } from "../features/spotify/spotifyAuth"
import { useEffect } from "react"
import { useNavigate } from "react-router-dom";

const SpotifyAuthCodeRedirect = () => {
  const [searchParams] = useSearchParams();
  const navigate = useNavigate();

  const code = searchParams.get("code") ?? "";
  const state = searchParams.get("state") ?? "";
  const redirectUri = window.origin + window.location.pathname;
  const receiveTime = new Date().toISOString();

  useEffect(() => {
    setOAuth2ResponseCode({ code, state, receiveTime, redirectUri }, window);
    navigate('/');
  }, []);

  return <div>success!</div>
}

export default SpotifyAuthCodeRedirect
