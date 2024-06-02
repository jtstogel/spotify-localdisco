import "./Layout.css"
import { Outlet, useNavigate } from "react-router-dom"
import Logo from '../logo.svg';
import SpotifyAccountIcon from '../components/SpotifyAccountIcon'

const Layout = () => {
  const navigate = useNavigate();
  const onLogoClick = () => {
    navigate(0);
  };

  return (
    <>
      <div className="topnav">
        <div className="logo-container">
          <img className="logo" src={Logo} alt="Local Disco Logo" width={64} onClick={onLogoClick} />
          <span>local disco</span>
        </div>
        <div className="spotify-account-icon"><SpotifyAccountIcon /></div>
      </div>
      <Outlet />
    </>
  )
}

export default Layout
