import { store } from "./app/store"
import { BrowserRouter, Routes, Route, useNavigate } from "react-router-dom"
import Layout from "./pages/Layout"
import Home from "./pages/Home"
import SpotifyAuthCodeRedirect from "./pages/SpotifyAuthCodeRedirect"
import SpotifyLogin from './pages/SpotifyLogin';
import { SPOTIFY_CODE_REDIRECT_PATH } from "./features/spotify/spotifyAuth"
import { useAppSelector } from "./app/hooks"
import { selectUserAuthenticated } from "./features/spotify/spotifySlice"
import { useEffect } from "react"

const App = () => {
  return (
    <BrowserRouter>
      <Routes>
        <Route path="/" element={<Layout />}>
          <Route index element={<Home />} />
          <Route
            path={SPOTIFY_CODE_REDIRECT_PATH}
            element={<SpotifyAuthCodeRedirect />}
          />
          <Route path="/spotify/login" element={<SpotifyLogin />}></Route>
        </Route>
      </Routes>
    </BrowserRouter>
  )
}

export default App
