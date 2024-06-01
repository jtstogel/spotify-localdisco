module Utils
  ( chunks,
    maybeHead,
  )
where

chunks :: Int -> [a] -> [[a]]
chunks 0 _ = error "cannot call chunks with non-postitive chunk size"
chunks _ [] = []
chunks n xs = take n xs : chunks n (drop n xs)

maybeHead :: [a] -> Maybe a
maybeHead [] = Nothing
maybeHead (x : _) = Just x
