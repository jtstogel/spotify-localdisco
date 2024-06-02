import styles from "./PlaylistConfigurationForm.module.css"
import type { FormEvent } from "react";
import { useState } from "react"

function padZeros(n: number, len: number) {
  return String(n).padStart(len, '0')
}

function dateString(d: Date): string {
  return [
    padZeros(d.getFullYear(), 4),
    padZeros(d.getMonth() + 1, 2),
    padZeros(d.getDate(), 2)
  ].join('-')
}

enum NewMusic {
  NONE = 'Only your artists',
  SOME = 'Some new artists',
  LOTS = 'Lots of new artists'
}

const NEW_MUSIC_SETTINGS = [NewMusic.NONE, NewMusic.SOME, NewMusic.LOTS];

const SPIDERING_DEPTHS = {
  [NewMusic.NONE]: 0,
  [NewMusic.SOME]: 20,
  [NewMusic.LOTS]: 50,
};

export interface PlaylistConfig {
  postalCode: string;
  radiusMiles: number,
  startTime: string;
  endTime: string;
  spideringDepth: number;
}

const PlaylistConfigurationForm = (
  { onSubmit }: {
    onSubmit: (form: PlaylistConfig) => void
  },
) => {
  const [postalCode, setPostalCode] = useState('94110');
  const [radiusMiles, setRadiusMiles] = useState('10');
  const [startDate, setStartDate] = useState(dateString(new Date()));
  const [endDate, setEndDate] = useState(dateString(new Date(new Date().getTime() + 120 * 24 * 60 * 60 * 1000)));
  const [newMusic, setNewMusic] = useState(NewMusic.SOME);

  const handleSubmit = (event: FormEvent) => {
    event.preventDefault()
    const startTime = new Date(startDate).toISOString()
    const endTime = new Date(endDate).toISOString()
    const spideringDepth = SPIDERING_DEPTHS[newMusic || NewMusic.SOME];

    onSubmit({
      radiusMiles: radiusMiles ? Number(radiusMiles) : 10,
      startTime,
      endTime,
      spideringDepth,
      postalCode,
    })
  }

  return (
    <div>
      <h4>Configure your playlist</h4>
      <form onSubmit={handleSubmit}>
        <div className={styles.row}>
          <label htmlFor="postalCode">Postal code (US only)</label>
          <input value={postalCode} name="postalCode" onChange={e => setPostalCode(e.target.value)} pattern="[0-9]*"></input>
        </div>
        <div className={styles.row}>
          <label htmlFor="radiusMiles">Concert distance (miles)</label>
          <input value={radiusMiles} name="radiumMiles" onChange={e => setRadiusMiles(e.target.value)} pattern="[0-9]*"></input>
        </div>
        <div className={styles.row}>
          <label htmlFor="startDate">Concert search start</label>
          <input value={startDate} name="startDate" onChange={e => setStartDate(e.target.value)} type="date"></input>
        </div>
        <div className={styles.row}>
          <label htmlFor="endDate">Concert search end</label>
          <input value={endDate} name="endDate" onChange={e => setEndDate(e.target.value)} type="date"></input>
        </div>
        <div className={styles.row}>
          <label htmlFor="spiderDepth">Music search</label>
          <div style={{ 'textAlign': 'left' }}>
            {
              NEW_MUSIC_SETTINGS.map(value =>
                <label key={value}>
                  <input value={value} name="spiderDepth" checked={newMusic === value} onChange={e => setNewMusic(e.target.value as NewMusic)} type="radio" />
                  {value}
                  <br />
                </label>
              )
            }
          </div>
        </div>
        <input type="submit" value="create playlist" className={styles['submit-button']}></input>
      </form>
    </div>
  )
}

export default PlaylistConfigurationForm
