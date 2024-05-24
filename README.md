# spotify-localdisco

You'll some environment variables w/ your credentials:

```
export SPOTIFY_CLIENT_ID={{YOUR_ID}}
export SPOTIFY_CLIENT_SECRET={{YOUR_SECRET}}
export TICKETMASTER_CONSUMER_KEY={{YOUR_KEY}}
export TICKETMASTER_CONSUMER_SECRET={{YOUR_SECRET}}
```

You'll need a Spotify developer account and a Ticketmaster key for their Discovery API.

Unzip `postal-codes.json.zip` and leave this in the working directory of the app.

Run with `stack run`.

The `web/` directory contains the web app that facilitates fetching credentials
and dispatching playlist creation requests.
