module DayOne (run) where

readInt :: String -> Int
readInt = read

requiredFuel m = let
    fuel = subtract 2 $ div m 3
  in 
    if fuel > 0 
      then fuel + requiredFuel fuel
      else 0

process = sum . map (requiredFuel . readInt) . lines

run = do
  input <- readFile "./inputs/day-1.txt"
  print $ process input