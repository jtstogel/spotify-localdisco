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

function dateString(d: Date): string {
  return d.toISOString().substring(0, 'YYYY-MM-DD'.length)
}

enum NewMusic {
  NONE = 'Only known artists',
  SOME = 'Some new artists',
  LOTS = 'Lots of new artists'
}

const NEW_MUSIC_SETTINGS = [NewMusic.NONE, NewMusic.SOME, NewMusic.LOTS];

const SPIDERING_DEPTHS = {
  [NewMusic.NONE]: 0,
  [NewMusic.SOME]: 20,
  [NewMusic.LOTS]: 50,
};

const Home = () => {
  const authToken = useAppSelector(selectAuthToken);
  const [createPlaylistJob, { data: response }] = useCreatePlaylistJobMutation()
  const navigate = useNavigate();
  const [postalCode, setPostalCode] = useState('94110');
  const [radiusMiles, setRadiusMiles] = useState('10');
  const [startDate, setStartDate] = useState(dateString(new Date()));
  const [endDate, setEndDate] = useState(dateString(new Date(new Date().getTime() + 120 * 24 * 60 * 60 * 1000)));
  const [newMusic, setNewMusic] = useState(NewMusic.SOME);

  if (!authToken) {
    useEffect(() => navigate('/spotify/login'));
    return <></>
  }

  const handleSubmit = (event: FormEvent) => {
    event.preventDefault()
    const startTime = new Date(startDate).toISOString()
    const endTime = new Date(endDate).toISOString()
    const spideringDepth = SPIDERING_DEPTHS[newMusic || NewMusic.SOME];

    createPlaylistJob({
      radiusMiles: radiusMiles ? Number(radiusMiles) : 10,
      startTime,
      endTime,
      spideringDepth,
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
            <label htmlFor="postalCode">Postal code</label>
            <input value={postalCode} name="postalCode" onChange={e => setPostalCode(e.target.value)} type="number"></input>
          </div>
          <div className="row">
            <label htmlFor="radiusMiles">Concert distance (miles)</label>
            <input value={radiusMiles} name="radiumMiles" onChange={e => setRadiusMiles(e.target.value)} type="number"></input>
          </div>
          <div className="row">
            <label htmlFor="startDate">Concert search start</label>
            <input value={startDate} name="startDate" onChange={e => setStartDate(e.target.value)} type="date"></input>
          </div>
          <div className="row">
            <label htmlFor="endDate">Concert search end</label>
            <input value={endDate} name="endDate" onChange={e => setEndDate(e.target.value)} type="date"></input>
          </div>
          <div className="row">
            <label htmlFor="spiderDepth">Music search</label>
            <div style={{'textAlign': 'left'}}>
              {
                NEW_MUSIC_SETTINGS.map(value =>
                  <label key={value}>
                    <input value={value} name="spiderDepth" checked={newMusic === value} onChange={e => setNewMusic(e.target.value as NewMusic)} type="radio" />
                    {value}
                    <br/>
                  </label>
                )
              }
            </div>
          </div>
          <input type="submit" value="create playlist" className="submit-button"></input>
        </form>
      </div>
    </div>
  )
}

export default Home
