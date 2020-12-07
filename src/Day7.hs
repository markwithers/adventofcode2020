{-# LANGUAGE ViewPatterns #-}

module Day7 where

import Data.List (unfoldr)
import qualified Data.Map as Map (Map, elems, findWithDefault, fromList, lookup)
import Data.Maybe (mapMaybe, maybeToList)

type Bag = (Int, String)

parse :: String -> Map.Map String [Bag]
parse = Map.fromList . mapMaybe (parseLine . words) . lines
  where
    parseLine (a : b : _ : _ : rest) = Just (a ++ b, unfoldr parseItems rest)
    parseLine _ = Nothing
    parseItems ((reads -> (n, "") : _) : a : b : _ : rest) = Just ((n, a ++ b), rest)
    parseItems _ = Nothing

goldFitsIn :: Map.Map String [Bag] -> Int
goldFitsIn bags = length . filter id $ Map.elems hasMatch
  where
    hasMatch = any (isMatch . snd) <$> bags
    isMatch "shinygold" = True
    isMatch item = Map.findWithDefault False item hasMatch

getMatchingBags :: String -> Map.Map String [Bag] -> [String]
getMatchingBags name bags = concatMap (uncurry replicate) $ concat $ maybeToList l
  where
    l = Map.lookup name bags

countBags :: Num a => String -> Map.Map String [Bag] -> a
countBags name bags = 1 + sum (map (`countBags` bags) $ getMatchingBags name bags)

day7 :: IO ()
day7 = do
  x <- readFile "./inputs/day7"
  let bags = parse x

  print . goldFitsIn $ bags
  print . subtract 1 $ countBags "shinygold" bags
