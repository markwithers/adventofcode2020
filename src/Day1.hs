module Day1 where

toInt :: String -> Int
toInt = read

day1 :: IO ()
day1 = do
  x <- readFile "./inputs/day1"
  let input = map toInt (lines x)
  print $ head [x * y | x <- input, y <- input, x + y == 2020]
  print $ head [x * y * z | x <- input, y <- input, z <- input, x + y + z == 2020]