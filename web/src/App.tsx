import { BrowserRouter, Routes, Route, Navigate } from "react-router-dom"
import Layout from "./pages/Layout"
import Home from "./pages/Home"
import SpotifyAuthCodeRedirect from "./pages/SpotifyAuthCodeRedirect"
import { SPOTIFY_OAUTH_REDIRECT_PATH } from "./features/spotify/spotifyOAuth"

const App = () => {
  return (
    <BrowserRouter>
      <Routes>
        <Route path="/" element={<Layout />}>
          <Route index element={<Home />} />
          <Route path={SPOTIFY_OAUTH_REDIRECT_PATH} element={<SpotifyAuthCodeRedirect />} />
          <Route path="*" element={<Navigate to="/" replace />} />
        </Route>
      </Routes>
    </BrowserRouter>
  )
}

export default App
