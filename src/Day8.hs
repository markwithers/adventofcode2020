module Day8 where

data Instruction = Nop Int | Acc Int | Jmp Int deriving (Show)

data Result = Cycles Int | Terminates Int deriving (Show)

parseLine :: [String] -> Instruction
parseLine ("nop" : ('+' : n) : _) = Nop (read n)
parseLine ("acc" : ('+' : n) : _) = Acc (read n)
parseLine ("jmp" : ('+' : n) : _) = Jmp (read n)
parseLine ("nop" : ('-' : n) : _) = Nop (negate $ read n)
parseLine ("acc" : ('-' : n) : _) = Acc (negate $ read n)
parseLine ("jmp" : ('-' : n) : _) = Jmp (negate $ read n)

replaceFn :: Int -> (a -> a) -> [a] -> [a]
replaceFn i fn is = take i is ++ [fn (is !! i)] ++ drop (i + 1) is

performInstruction :: Int -> [Int] -> Int -> [Instruction] -> Result
performInstruction acc seen index is
  | index >= length is = Terminates acc
  | index `elem` seen = Cycles acc
  | otherwise = case i of
    (Nop _) -> performInstruction acc (seen ++ [index]) (index + 1) is
    (Acc n) -> performInstruction (acc + n) (seen ++ [index]) (index + 1) is
    (Jmp n) -> performInstruction acc (seen ++ [index]) (index + n) is
  where
    i = is !! index

swap :: Instruction -> Instruction
swap i = case i of
  (Nop n) -> Jmp n
  (Jmp n) -> Nop n
  (Acc n) -> Acc n

modifyInstructions :: [Instruction] -> [[Instruction]]
modifyInstructions is = map (\i -> replaceFn i swap is) [0 .. length is - 1]

terminates :: Result -> Bool
terminates (Terminates _) = True
terminates (Cycles _) = False

day8 :: IO ()
day8 = do
  x <- readFile "./inputs/day8"
  print . performInstruction 0 [] 0 . map (parseLine . words) $ lines x
  print
    . filter terminates
    . map (performInstruction 0 [] 0)
    . modifyInstructions
    . map (parseLine . words)
    $ lines x
