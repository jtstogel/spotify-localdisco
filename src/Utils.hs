module Utils
  ( chunks,
  )
where

chunks :: Int -> [a] -> [[a]]
chunks 0 _ = error "cannot call chunks with non-postitive chunk size"
chunks _ [] = []
chunks n xs = take n xs : chunks n (drop n xs)
