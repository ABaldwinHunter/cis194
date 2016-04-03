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

doubleEvens :: Int -> Int -> Int
doubleEvens n i
  | (i `mod` 2) == 0 = 2 * n
  | otherwise = n

doubleEveryOther :: [Int] -> [Int]
doubleEveryOther n = mapWithIndex doubleEvens n

--doubleEveryOther :: [Integer] -> [Integer]
--doubleEveryOther n
--  | length(n) `mod` 2 == 0 = doubleOdds(n)
--  | otherwise = doubleEvens(n)

-- improved formatting
--doubleEvens :: [Integer] -> [Integer]
--doubleEvens [] = []
--doubleEvens [x] = []
--doubleEvens (x:(y:ys)) = [ double(x) ] ++ [ y ] ++ doubleEvens (ys)

--original formatting
--doubleOdds :: [Integer] -> [Integer]
--doubleOdds [] = []
--doubleOdds n = [head(n)] ++ [double(head(tail n))] ++ doubleOdds(tail (tail n))

--double :: Integer -> Integer
--double n = 2 * n
