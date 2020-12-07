module Day6 where

import Data.List
import Data.List.Split

day6 :: IO ()
day6 = do
  x <- readFile "./inputs/day6"
  let groups = splitWhen (== "") $ lines x

  print . sum $ map (length . foldr1 union) groups
  print . sum $ map (length . foldr1 intersect) groups
