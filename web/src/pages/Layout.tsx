import "./Layout.css"
import { Outlet } from "react-router-dom"
import Logo from '../logo.svg';
import SpotifyAccountIcon from '../components/SpotifyAccountIcon'

const Layout = () => {
  return (
    <>
      <div className="topnav">
        <div className="logo-container">
          <img className="logo" src={Logo} alt="Local Disco Logo" width={64} />
          <span>Local Disco</span>
        </div>
        <div className="spotify-account-icon"><SpotifyAccountIcon /></div>
      </div>
      <Outlet />
    </>
  )
}

export default Layout
