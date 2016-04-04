toDigits :: Integer -> [Integer]
toDigits n
  | n <= 0 = []
  | otherwise = toDigits (n `div` 10) ++ [n `mod` 10]

toDigitsRev :: Integer -> [Integer]
toDigitsRev n = reverse (toDigits n)

mapWithIndex :: (a -> Int -> b) -> [a] -> [b]
mapWithIndex x = go x 0
  where
    go :: (a -> Int -> b) -> Int -> [a] -> [b]
    go _ _ [] = []
    go f i (x:xs) = f x i : go f (i + 1) xs

-- unused implementation of map. Just practicing.
--myMap :: (a -> b) -> [a] -> [b]
--myMap _ [] = []
--myMap f (x:xs) = f x : myMap f xs

doubleEveryOther :: [Int] -> [Int]
doubleEveryOther n = mapWithIndex doubleEvens n
  where
    doubleEvens :: Int -> Int -> Int
    doubleEvens n i
      | (i `mod` 2) == 0 = n
      | otherwise = 2 * n

