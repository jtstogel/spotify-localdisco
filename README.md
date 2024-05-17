# spotify-localdisco


Set up a `.env` file with your credentials. Should look like:

```
SPOTIFY_CLIENT_ID={{YOUR_ID}}
SPOTIFY_CLIENT_SECRET={{YOUR_SECRET}}
TICKETMASTER_CONSUMER_KEY={{YOUR_KEY}}
TICKETMASTER_CONSUMER_SECRET={{YOUR_SECRET}}
```

You'll need a Spotify developer account and a Ticketmaster key for their Discovery API.

Unzip `postal-codes.json.zip`. Leave this in the same directory as `.env`.

The app will use this file to translate `postal code` -> `lat,lon` -> `geoHash`,
which is what is required by the Ticketmaster Discovery API.

The `web/` directory contains the web app that facilitates fetching credentials
and dispatching playlist creation requests.