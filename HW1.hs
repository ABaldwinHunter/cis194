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

doubleEveryOther :: [Int] -> [Int]
doubleEveryOther [] = []
doubleEveryOther n = reverse (mapWithIndex applyDoubling (reverse n))
  where
    applyDoubling :: Int -> Int -> Int
    applyDoubling n i
      | (i `mod` 2) == 0 = n
      | otherwise = 2 * n

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits n = sumValues (allTheSingleLadies n)
  where
    allTheSingleLadies :: [Integer] -> [Integer]
    allTheSingleLadies [] = []
    allTheSingleLadies (x:xs) = (toDigits x) ++ (allTheSingleLadies xs)

    sumValues :: [Integer] -> Integer
    sumValues [] = 0
    sumValues (x:xs) = x + sumValues xs
