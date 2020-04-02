module DayFour
  ( run
  )
where

import           Data.List.Split

hasAdjecent :: Integral a => [a] -> Bool
hasAdjecent [_     ] = False
hasAdjecent (x : xs) = x == head xs || hasAdjecent xs

isIncreasingOrSame :: Integral a => [a] -> Bool
isIncreasingOrSame [_     ] = True
isIncreasingOrSame (x : xs) = x <= head xs && isIncreasingOrSame xs

readInt :: String -> Int
readInt = read

toDigits :: Show a => a -> [Int]
toDigits = map readInt . tail . splitOn "" . show

findPossiblePasswords :: Int -> Int -> [[Int]]
findPossiblePasswords start end =
  filter isIncreasingOrSame . filter hasAdjecent . map toDigits $ [start .. end]

solvePartOne = length $ findPossiblePasswords 145852 616942

group :: [[Int]] -> [Int] -> [[Int]]
group ys []       = ys
group [] (x : xs) = group [[x]] xs
group ys (x : xs) = if head (last ys) == x
  then group (init ys ++ [last ys ++ [x]]) xs
  else group (ys ++ [[x]]) xs

hasLengthTwo :: [a] -> Bool
hasLengthTwo xs = length xs == 2

hasOnePairAdjacentMatching :: [Int] -> Bool
hasOnePairAdjacentMatching = any hasLengthTwo . group []

solvePartTwo = length
  $ filter hasOnePairAdjacentMatching (findPossiblePasswords 145852 616942)

run = solvePartTwo
