# spotify-localdisco

Maybe live version: https://disco.stowgulls.com

## Building and running

You'll need a Spotify developer account and a Ticketmaster key for their Discovery API.

Once you have those, write a `.env` file with your credentials:

```
export SPOTIFY_CLIENT_ID={{YOUR_ID}}
export SPOTIFY_CLIENT_SECRET={{YOUR_SECRET}}
export TICKETMASTER_CONSUMER_KEY={{YOUR_KEY}}
```

Then build and run with:

```
chmod +x ./build.sh

./build.sh
source .env
docker run -e SPOTIFY_CLIENT_ID -e SPOTIFY_CLIENT_SECRET -e TICKETMASTER_CONSUMER_KEY -p 0.0.0.0:8080:8080 localdisco
```

To run the program without docker:

1. Unzip `postal-codes.json.zip` and leave `postal-codes.json` in the working directory of the app.
2. Run with `stack run`.

To run the web portion of the app:

```
cd web
npm run dev
```

## TODO

- [ ] Make the created playlist private. IDK why it's public rn.
- [ ] Exclude the playlist from the user's taste profile so listening to it doesn't affect recommendations.
- [ ] Sign up for Extended Quota w spotify
- [ ] Overwrite the existing playlist instead of always making a new one.
- [ ] Build inside docker for reproducible builds.
