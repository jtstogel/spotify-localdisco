type Path = `/${string}`

const API_URL = 'http://localhost:8080';

export async function get<T>(
  path: Path,
  query?: Array<[string, string]>,
): Promise<T> {
  const searchParams = new URLSearchParams(query ?? []).toString()
  const queryPart = searchParams ? `?${searchParams}` : ""
  const response = await fetch(`${API_URL}${path}${queryPart}`, {
    mode: "cors",
    method: "GET",
  })
  return await response.json()
}

export async function post<T>(path: Path, body?: string): Promise<T> {
  const response = await fetch(API_URL + path, {
    mode: "cors",
    method: "POST",
    body,
  })
  return response.json()
}

export interface PromiseResolver<T> {
  promise: Promise<T>
  resolve: (value: T | PromiseLike<T>) => void
  reject: (reason?: any) => void
}

export function promiseResolver<T>(): PromiseResolver<T> {
  let resolve: PromiseResolver<T>["resolve"] = () => {}
  let reject: PromiseResolver<T>["reject"] = () => {}
  const promise = new Promise<T>((innerResolve, innerReject) => {
    resolve = innerResolve
    reject = innerReject
  })
  return { promise, resolve, reject }
}
