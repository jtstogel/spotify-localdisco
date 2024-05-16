import type { Action, ThunkAction } from "@reduxjs/toolkit"
import { combineSlices, configureStore } from "@reduxjs/toolkit"
import { setupListeners } from "@reduxjs/toolkit/query"
import { spotifySlice } from "../features/spotify/spotifySlice"
import { spotifyAccountsApiSlice } from "../features/spotify/spotifyAccountsApiSlice"
import { spotifyApiSlice } from "../features/spotify/spotifyApiSlice"
import { apiSlice } from '../features/api/apiSlice'

function withDebounce(f: () => void, debounceMs: number) {
  let timeout: number | null = null;
  return () => {
    if (timeout !== null) return null;
    timeout = setTimeout(() => {
      timeout = null;
      f();
    }, debounceMs)
  };
}

const STORE_KEY = 'reduxStore';

export const saveStore = () => {
  return localStorage.setItem(STORE_KEY, JSON.stringify(store.getState()));
}

const loadStore = () => {
  return JSON.parse(localStorage.getItem(STORE_KEY) ?? 'null') ?? undefined
};

// `combineSlices` automatically combines the reducers using
// their `reducerPath`s, therefore we no longer need to call `combineReducers`.
const rootReducer = combineSlices(apiSlice, spotifySlice, spotifyApiSlice, spotifyAccountsApiSlice)
// Infer the `RootState` type from the root reducer
export type RootState = ReturnType<typeof rootReducer>

// The store setup is wrapped in `makeStore` to allow reuse
// when setting up tests that need the same store config
export const makeStore = (preloadedState?: Partial<RootState>) => {
  const store = configureStore({
    reducer: rootReducer,
    // Adding the api middleware enables caching, invalidation, polling,
    // and other useful features of `rtk-query`.
    middleware: getDefaultMiddleware => {
      return getDefaultMiddleware()
        .concat(spotifyAccountsApiSlice.middleware)
        .concat(spotifyApiSlice.middleware)
        .concat(apiSlice.middleware)
    },
    preloadedState,
  })
  // configure listeners using the provided defaults
  // optional, but required for `refetchOnFocus`/`refetchOnReconnect` behaviors
  setupListeners(store.dispatch)
  return store
}

export const store = makeStore(loadStore())
store.subscribe(withDebounce(saveStore, /*debounceMs=*/100));

// Infer the type of `store`
export type AppStore = typeof store
// Infer the `AppDispatch` type from the store itself
export type AppDispatch = AppStore["dispatch"]
export type AppThunk<ThunkReturnType = void> = ThunkAction<
  ThunkReturnType,
  RootState,
  unknown,
  Action
>
