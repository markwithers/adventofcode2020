{-# LANGUAGE FlexibleContexts #-}

module Day2 where

import Text.Regex.TDFA (Regex, RegexContext, (=~))

toInt :: String -> Int
toInt = read

decode :: (a, b, c, [String]) -> (Int, Int, Char, String)
decode (_, _, _, gs) = (toInt $ head gs, toInt $ gs !! 1, head (gs !! 2), gs !! 3)

count :: Eq a => a -> [a] -> Int
count a = length . filter (a ==)

isValid1 :: (Int, Int, Char, String) -> Bool
isValid1 (n1, n2, c, pwd) = s >= n1 && s <= n2
  where
    s = count c pwd

isValid2 :: (Int, Int, Char, String) -> Bool
isValid2 (n1, n2, c, pwd) = c1 /= c2 && (c1 == c || c2 == c)
  where
    c1 = pwd !! (n1 -1)
    c2 = pwd !! (n2 -1)

parse :: RegexContext Regex source1 (String, String, String, [String]) => source1 -> (String, String, String, [String])
parse s = s =~ "([0-9]*)-([0-9]*) (.): (.*)" :: (String, String, String, [String])

day2 :: IO ()
day2 = do
  x <- readFile "./inputs/day2"
  print . length . filter (True ==) $ map (isValid1 . decode . parse) (lines x)
  print . length . filter (True ==) $ map (isValid2 . decode . parse) (lines x)
