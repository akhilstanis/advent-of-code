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

findPossiblePasswords start end =
  length
    . filter isIncreasingOrSame
    . filter hasAdjecent
    . map toDigits
    $ [start .. end]

run = findPossiblePasswords 145852 616942
