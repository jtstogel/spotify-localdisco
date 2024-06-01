import { useSearchParams } from "react-router-dom"
import { useEffect } from "react"
import { useNavigate } from "react-router-dom";
import { authCodeReceived, selectUserAuthenticated } from "../features/spotify/spotifySlice";
import { useAppDispatch, useAppSelector } from "../app/hooks";
import '../styles/spinner.css'
import './SpotifyAuthCodeRedirect.css';

const SpotifyAuthCodeRedirect = () => {
  const [searchParams] = useSearchParams();
  const isAuthenticated = useAppSelector(selectUserAuthenticated);
  const navigate = useNavigate();
  const dispatch = useAppDispatch();

  useEffect(() => {
    if (searchParams.get("error")) {
      navigate('/');
      return;
    }

    const code = searchParams.get("code") ?? "";
    dispatch(authCodeReceived(code));
  }, [searchParams, dispatch, navigate]);

  useEffect(() => {
    if (isAuthenticated) {
      navigate('/');
    }
  }, [navigate, isAuthenticated]);

  return <div>
    <div className="spinner-container"><span className="spinner"></span></div>
  </div>
}

export default SpotifyAuthCodeRedirect
