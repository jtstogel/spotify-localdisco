export enum LocalStorageKey {
    CODE = 'spotify/authCode',
    TOKENS = 'spotify/authTokens'
}

export function getItem<T>(w: Window, key: LocalStorageKey): T | null {
    return JSON.parse(w.localStorage.getItem(key) ?? "null") as T | null;
}

export function setItem<T>(w: Window, key: LocalStorageKey, item: T | null) {
    w.localStorage.setItem(key, JSON.stringify(item));
}
