module Day5 where

partition :: Integral a => Char -> (a, a) -> (a, a)
partition instruction (min, max)
  | instruction == 'R' || instruction == 'B' = (min + ((max - min + 1) `div` 2), max)
  | otherwise = (min, max - ((max - min + 1) `div` 2))

getPos :: Integral p => String -> (p, p) -> p
getPos (i : is) ns = getPos is (partition i ns)
getPos [] (_, n) = n

findSeat :: Integral a => String -> a
findSeat is = getPos (take 7 is) (0, 127) * 8 + getPos (drop 7 is) (0, 7)

day5 :: IO ()
day5 = do
  x <- readFile "./inputs/day5"
  let ids = map findSeat $ lines x

  print . maximum $ ids
  print $ [id + 1 | id <- ids, id `elem` ids, (id + 1) `notElem` ids, (id + 2) `elem` ids]
