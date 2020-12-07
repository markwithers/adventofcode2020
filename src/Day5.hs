module Day5 where

partition :: Integral a => Char -> (a, a) -> (a, a)
partition instruction (min, max)
  | instruction == 'R' || instruction == 'B' = (min + ((max - min + 1) `div` 2), max)
  | otherwise = (min, max - ((max - min + 1) `div` 2))

getPos :: Integral p => String -> (p, p) -> p
getPos (i : is) ns = getPos is (partition i ns)
getPos [] (_, n) = n

findSeat is = getPos (take 7 is) (0, 127) * 8 + getPos (drop 7 is) (0, 7)

day5 :: IO ()
day5 = do
  x <- readFile "./inputs/day5"
  print . maximum . map findSeat $ lines x
