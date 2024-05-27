import "./Home.css"
import { FormEvent, useEffect, useState } from "react"
import { useNavigate } from "react-router-dom"
import { selectAuthToken } from "../features/spotify/spotifySlice"
import { useAppSelector } from "../app/hooks"
import { useCreatePlaylistJobMutation, useGetPlaylistJobQuery } from "../features/api/apiSlice"

const CreatePlaylistStatus = ({ name }: { name: string }) => {
  const [poll, setPoll] = useState(true);
  const [statuses, setStatuses] = useState<string[]>([]);

  const { data, error } = useGetPlaylistJobQuery({ name }, {
    pollingInterval: poll ? 500 : 0,
  });

  const message = data?.metadata?.message;
  if (message && message !== statuses[statuses.length - 1]) {
    setStatuses(statuses.concat([message]))
  }

  if (!data?.done) {
    const succeededStatuses = statuses.slice(0, statuses.length - 1);
    const pendingStatus = statuses[statuses.length - 1];

    return <div style={{ padding: '24px' }}>
      <h4>We're doing things...</h4>
      <ul>
        {succeededStatuses.map(m => <li>{m} &#10003;</li>)}
        {pendingStatus ? <li><span className="loading">{pendingStatus}</span></li> : <></>}
      </ul>
    </div>
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


  return <div style={{ padding: '24px' }}>
    <h4>Artists playing soon near you</h4>
    <a href={data.result?.playlistLink}>Click to see your playlist</a>
    <ul>
      {data.result?.artists?.map(a => <li>{a}</li>)}
    </ul>
  </div>
}

const Home = () => {
  const authToken = useAppSelector(selectAuthToken);
  const [createPlaylistJob, { data: response }] = useCreatePlaylistJobMutation()
  const navigate = useNavigate();
  const [postalCode, setPostalCode] = useState('94110');
  const [radiusMiles, setRadiusMiles] = useState('10');
  const [days, setDays] = useState('120');
  const [spideringDepth, setSpideringDepth] = useState('20');

  if (!authToken) {
    useEffect(() => navigate('/spotify/login'));
    return <></>
  }

  const handleSubmit = (event: FormEvent) => {
    event.preventDefault()
    console.log(event, postalCode, radiusMiles, days)
    createPlaylistJob({
      radiusMiles: radiusMiles ? Number(radiusMiles) : 10,
      days: days ? Number(days) : 30,
      spideringDepth: spideringDepth ? Number(spideringDepth) : 1,
      postalCode,
      authToken,
    })
  }

  if (response) {
    return <div className="form-box"><CreatePlaylistStatus name={response.name}></CreatePlaylistStatus></div>
  }

  return (
    <div className="App">
      <div className="form-box">
        <h4>Configure your playlist</h4>
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
          <div className="row">
            <label htmlFor="spiderDepth">Spider depth</label>
            <input value={spideringDepth} name="spiderDepth" onChange={e => setSpideringDepth(e.target.value)} type="number"></input>
          </div>
          <input type="submit" value="create playlist" className="submit-button"></input>
        </form>
      </div>
    </div>
  )
}

export default Home
