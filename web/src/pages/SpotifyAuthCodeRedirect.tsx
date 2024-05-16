import { useSearchParams } from "react-router-dom"
import { useEffect } from "react"
import { useNavigate } from "react-router-dom";
import { authCodeReceived, selectUserAuthenticated } from "../features/spotify/spotifySlice";
import { useAppDispatch, useAppSelector } from "../app/hooks";

const SpotifyAuthCodeRedirect = () => {
  const [searchParams] = useSearchParams();
  const isAuthenticated = useAppSelector(selectUserAuthenticated);
  const navigate = useNavigate();
  const dispatch = useAppDispatch();

  useEffect(() => {
    const code = searchParams.get("code") ?? "";
    dispatch(authCodeReceived(code));
  }, [searchParams, dispatch]);

  useEffect(() => {
    if (isAuthenticated) {
      navigate('/');
    }
  }, [navigate, isAuthenticated]);

  return <div>authenticating...</div>
}

export default SpotifyAuthCodeRedirect
