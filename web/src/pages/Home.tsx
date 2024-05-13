import "./Home.css"
import { Counter } from "../features/counter/Counter"
import { Quotes } from "../features/quotes/Quotes"
import logo from "../logo.svg"
import { useEffect } from "react"
import * as spotifyAuth from "../features/spotify/spotifyAuth"
import { store } from "../app/store"
import { BrowserRouter, Routes, Route } from "react-router-dom"

const Home = () => {
  useEffect(() => {
    window.addEventListener(
      "storage",
      spotifyAuth.localStorageListener(store, window),
      false,
    )
  }, [])

  return (
    <div className="App">
    </div>
  )
}

export default Home
