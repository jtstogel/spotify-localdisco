import { useSearchParams } from "react-router-dom"
import { useEffect } from "react"
import { useNavigate } from "react-router-dom";
import { selectUserAuthenticated } from "../features/spotify/spotifySlice";
import { useSelector } from "react-redux";
import { useExchangeCodeMutation } from "../features/spotify/spotifyApiSlice";

const exchangedCodes = new Set<string>();

const SpotifyAuthCodeRedirect = () => {
  const [searchParams] = useSearchParams();
  const [exchangeCode] = useExchangeCodeMutation();
  const isAuthenticated = useSelector(selectUserAuthenticated);
  const navigate = useNavigate();

  useEffect(() => {
    const code = searchParams.get("code") ?? "";
    const redirectUri = window.origin + window.location.pathname;
    if (!exchangedCodes.has(code)) {
      exchangedCodes.add(code)
      exchangeCode({ code, redirectUri }).unwrap()
    }
  }, [searchParams]);

  useEffect(() => {
    if (isAuthenticated) {
      navigate('/');
    }
  }, [navigate, isAuthenticated]);

  return <div>authenticating...</div>
}

export default SpotifyAuthCodeRedirect
