module Day8 where

data Instruction = Nop Int | Acc Int | Jmp Int deriving (Show)

parseLine :: [String] -> Instruction
parseLine ("nop" : ('+' : n) : _) = Nop (read n)
parseLine ("acc" : ('+' : n) : _) = Acc (read n)
parseLine ("jmp" : ('+' : n) : _) = Jmp (read n)
parseLine ("nop" : ('-' : n) : _) = Nop (negate $ read n)
parseLine ("acc" : ('-' : n) : _) = Acc (negate $ read n)
parseLine ("jmp" : ('-' : n) : _) = Jmp (negate $ read n)

performInstruction :: Int -> [Int] -> [Instruction] -> Int -> Int
performInstruction acc seen is index
  | index `elem` seen = acc
  | otherwise = case i of
    (Nop _) -> performInstruction acc (seen ++ [index]) is (index + 1)
    (Acc n) -> performInstruction (acc + n) (seen ++ [index]) is (index + 1)
    (Jmp n) -> performInstruction acc (seen ++ [index]) is (index + n)
  where
    i = is !! index

day8 :: IO ()
day8 = do
  x <- readFile "./inputs/day8"
  print . (\is -> performInstruction 0 [] is 0) . map (parseLine . words) $ lines x
