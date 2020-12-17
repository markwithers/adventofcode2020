module Day9 where

import Data.List (tails, transpose)

sumPairs :: Num a => [a] -> [a]
sumPairs l = [x + y | (x : ys) <- tails l, y <- ys]

windows :: Int -> [a] -> [[a]]
windows m = transpose . take m . tails

part1 :: [Int] -> Int
part1 input = head [i | (i, w) <- zip (drop 25 input) (windows 25 input), i `notElem` sumPairs w]

part2 :: [Int] -> Int
part2 input = head $ concatMap (\s -> [minimum w + maximum w | w <- windows s input, sum w == part1 input]) [2 .. length input]

day9 :: IO ()
day9 = do
  x <- readFile "./inputs/day9"
  let input = lines x
  print . part1 . map read $ input
  print . part2 . map read $ input
