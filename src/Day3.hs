module Day3 where

count :: Num a => [[a]] -> Int -> Int -> a
count trees x y =
  sum
    [cycle (trees !! y) !! x | (x, y) <- zip [0, x ..] [0, y .. length trees -1]]

day3 :: IO ()
day3 = do
  x <- readFile "./inputs/day3"
  let countTrees = count $ map (map (\x -> if x == '#' then 1 else 0)) $ lines x
  print $ countTrees 3 1
  print $
    product
      [countTrees 1 1, countTrees 3 1, countTrees 5 1, countTrees 7 1, countTrees 1 2]
