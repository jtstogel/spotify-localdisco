module GeoHash
    (
        geoHash
    )
    where

import Data.Array 

geoHashLength :: Int
geoHashLength = 12

encodePoint :: (Fractional a, Ord a) => (a, a) -> Int -> a -> [Bool]
encodePoint _ 0 _ = []
encodePoint (lower, upper) resolution value = if value > middle
    then True  : encodePoint (middle, upper) (resolution - 1) value
    else False : encodePoint (lower, middle) (resolution - 1) value
    where middle = (lower + upper) / 2


-- Interleaves two lists, discarding extra elements if lengths mismatch.
interleave :: [a] -> [a] -> [a]
interleave x y = concat $ zipWith ((. return) . (:)) x y

chunks :: Int -> [a] -> [[a]]
chunks 0 _ = error "cannot call chunks with non-postitive chunk size"
chunks _ [] = []
chunks n xs = (take n xs) : chunks n (drop n xs)

alphabet :: Array Int Char
alphabet = listArray (0, length a - 1) a
  where a = "0123456789bcdefghjkmnpqrstuvwxyz"

boolsToInt :: [Bool] -> Int
boolsToInt = foldl (\ acc v -> (acc * 2 + (if v then 1 else 0))) 0

geoHashWithResolution :: (Fractional a, Ord a) => Int -> (a, a) -> String
geoHashWithResolution size (lat, lon) = map (alphabet !)
        $ map boolsToInt
        $ chunks 5
        $ interleave (encodePoint (-180, 180) res lon) (encodePoint (-90, 90) res lat)
    where
        res = (size * 5 + 1) `div` 2

-- Computes the geo hash of the provided coordinates.
-- Only expects (lat, lon) within ranges [-90, 90] and [-180, 180].
geoHash :: (Fractional a, Ord a) => (a, a) -> String
geoHash = geoHashWithResolution geoHashLength

