import { useEffect } from "react"
import * as spotifyAuth from "./features/spotify/spotifyAuth"
import { store } from "./app/store"
import { BrowserRouter, Routes, Route } from "react-router-dom"
import Layout from "./pages/Layout"
import Home from "./pages/Home"
import SpotifyAuthCodeRedirect from "./components/SpotifyAuthCodeRedirect"
import { SPOTIFY_CODE_REDIRECT_PATH } from "./features/spotify/spotifyAuth"

const App = () => {
  useEffect(() => {
    window.addEventListener(
      "storage",
      spotifyAuth.localStorageListener(store, window),
      false,
    )
  }, [])

  return (
    <BrowserRouter>
      <Routes>
        <Route path="/" element={<Layout />}>
          <Route index element={<Home />} />
          <Route
            path={SPOTIFY_CODE_REDIRECT_PATH}
            element={<SpotifyAuthCodeRedirect />}
          />
        </Route>
      </Routes>
    </BrowserRouter>
  )
}

export default App
