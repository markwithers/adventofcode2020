module Day4 where

import Data.List (find)
import Data.List.Split (splitOn, splitWhen)
import Data.Maybe (isJust)
import Text.Regex.TDFA ((=~))

expected :: [(String, String)]
expected =
  [ ("byr", "^(200[0-2]|19[2-9][0-9])$"),
    ("iyr", "^(2020|201[0-9])$"),
    ("eyr", "^(2030|202[0-9])$"),
    ("hgt", "^((1([5-8][0-9]|9[0-3])cm)|((59|6[0-9]|7[0-6])in))$"),
    ("hcl", "^(#[0-9a-f]{6})$"),
    ("ecl", "^(amb|blu|brn|gry|grn|hzl|oth)$"),
    ("pid", "^[0-9]{9}$")
  ]

keyValues :: [String] -> [[String]]
keyValues = map (splitOn ":")

mMatchVal :: String -> Maybe [String] -> Bool
mMatchVal regex m = case m of
  Just [_, v] -> v =~ regex
  Nothing -> False

validate1 :: Foldable t => t [String] -> Bool
validate1 ks = all (\(key, _) -> isJust (find ((== key) . head) ks)) expected

validate2 :: Foldable t => t [String] -> Bool
validate2 ks = all (\(key, regex) -> mMatchVal regex (find ((== key) . head) ks)) expected

day4 :: IO ()
day4 = do
  x <- readFile "./inputs/day4"
  let keyValuePairs = map (keyValues . concatMap (splitOn " ")) $ splitWhen (== "") $ lines x

  print . length . filter validate1 $ keyValuePairs
  print . length . filter validate2 $ keyValuePairs
