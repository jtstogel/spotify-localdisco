import { BrowserRouter, Routes, Route } from "react-router-dom"
import Layout from "./pages/Layout"
import Home from "./pages/Home"
import SpotifyAuthCodeRedirect from "./pages/SpotifyAuthCodeRedirect"
import SpotifyLogin from './pages/SpotifyLogin';
import { SPOTIFY_OAUTH_REDIRECT_PATH } from "./features/spotify/spotifyOAuth"

const App = () => {
  return (
    <BrowserRouter>
      <Routes>
        <Route path="/" element={<Layout />}>
          <Route index element={<Home />} />
          <Route
            path={SPOTIFY_OAUTH_REDIRECT_PATH}
            element={<SpotifyAuthCodeRedirect />}
          />
          <Route path="/spotify/login" element={<SpotifyLogin />}></Route>
        </Route>
      </Routes>
    </BrowserRouter>
  )
}

export default App
