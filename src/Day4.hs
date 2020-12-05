module Day4 where

import Data.List (find)
import Data.List.Split (splitOn, splitWhen)
import Data.Sort (sort)
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

keys :: [String] -> [String]
keys = map (head . splitOn ":")

keyValues :: [String] -> [[String]]
keyValues = map (splitOn ":")

validate :: [String] -> Bool
validate ks
  | sort ks == ["byr", "cid", "ecl", "eyr", "hcl", "hgt", "iyr", "pid"] = True
  | sort ks == ["byr", "ecl", "eyr", "hcl", "hgt", "iyr", "pid"] = True
  | otherwise = False

match :: String -> String -> Bool
match regex x = x =~ regex

mMatchVal :: String -> Maybe [String] -> Bool
mMatchVal regex m = case m of
  Just [k, v] -> v =~ regex
  Nothing -> False

validate2 :: Foldable t => t [String] -> Bool
validate2 ks = all (\(key, regex) -> mMatchVal regex (find ((== key) . head) ks)) expected

day4 :: IO ()
day4 = do
  x <- readFile "./inputs/day4"
  let rawValues = map (concatMap (splitOn " ")) $ splitWhen (== "") $ lines x

  print . length . filter validate . map keys $ rawValues
  print . length . filter validate2 . map keyValues $ rawValues
