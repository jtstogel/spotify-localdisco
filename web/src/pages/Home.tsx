import "./Home.css"
import { FormEvent, useEffect, useState } from "react"
import { useNavigate } from "react-router-dom"
import { selectAuthTokens } from "../features/spotify/spotifySlice"
import { useAppSelector } from "../app/hooks"
import { useCreatePlaylistJobMutation, useGetPlaylistJobQuery } from "../features/api/apiSlice"

const CreatePlaylistStatus = ({ name }: { name: string }) => {
  const [poll, setPoll] = useState(true);
  const { data, error } = useGetPlaylistJobQuery({ name }, {
    pollingInterval: poll ? 500 : 0,
    skipPollingIfUnfocused: true,
  });

  if (!data?.done) {
    return <div className="loading">{data?.metadata?.message}</div>
  }

  if (poll) {
    setPoll(false);
  }

  if (error || data?.error) {
    return <div>
      Failed to get playlist status... Please try again later!
      <div>{JSON.stringify(error || data?.error)}</div>
    </div>
  }

  return <div>{JSON.stringify(data.result)}</div>
}

const Home = () => {
  const authTokens = useAppSelector(selectAuthTokens);
  const [createPlaylistJob, { data: response }] = useCreatePlaylistJobMutation()
  const navigate = useNavigate();
  const [postalCode, setPostalCode] = useState('94110');
  const [radiusMiles, setRadiusMiles] = useState('10');
  const [days, setDays] = useState('120');

  useEffect(() => {
    if (!authTokens?.accessToken) {
      navigate('/spotify/login')
    }
  }, [authTokens?.accessToken, navigate])

  const handleSubmit = (event: FormEvent) => {
    event.preventDefault()
    console.log(event, postalCode, radiusMiles, days)
    const spotifyAccessToken = authTokens?.accessToken!;
    createPlaylistJob({
      radiusMiles: radiusMiles ? Number(radiusMiles) : 10,
      days: days ? Number(days) : 30,
      postalCode,
      spotifyAccessToken
    })
  }

  if (response) {
    return <><CreatePlaylistStatus name={response.name}></CreatePlaylistStatus></>
  }

  return (
    <div className="App">
      <div className="form-box">
        <h4>Create new playlist</h4>
        <form onSubmit={handleSubmit}>
          <div className="row">
            <label htmlFor="postalCode">Postal code (US only)</label>
            <input value={postalCode} name="postalCode" onChange={e => setPostalCode(e.target.value)} type="number"></input>
          </div>
          <div className="row">
            <label htmlFor="radiusMiles">Search radius (miles)</label>
            <input value={radiusMiles} name="radiumMiles" onChange={e => setRadiusMiles(e.target.value)} type="number"></input>
          </div>
          <div className="row">
            <label htmlFor="days">Days</label>
            <input value={days} name="days" onChange={e => setDays(e.target.value)} type="number"></input>
          </div>
          <input type="submit" value="Create playlist" className="submit-button"></input>
        </form>
      </div>
    </div>
  )
}

export default Home
