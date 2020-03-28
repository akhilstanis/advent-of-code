module DayThree
  ( run
  )
where

import           Data.List.Split
import qualified Data.Set
import           Data.List

readInt :: String -> Int
readInt = read

parseMove :: [Char] -> (Char, Int)
parseMove (direction : steps) = (direction, readInt steps)

parseLine :: [Char] -> [(Char, Int)]
parseLine = map parseMove . splitOn ","

parseInput :: String -> [[(Char, Int)]]
parseInput = map parseLine . lines

generatePath :: Integral a => (a, a) -> (Char, a) -> [(a, a)]
generatePath (x, y) ('U', steps) = [ (x, y + y') | y' <- [1 .. steps] ]
generatePath (x, y) ('R', steps) = [ (x + x', y) | x' <- [1 .. steps] ]
generatePath (x, y) ('D', steps) = [ (x, y - y') | y' <- [1 .. steps] ]
generatePath (x, y) ('L', steps) = [ (x - x', y) | x' <- [1 .. steps] ]
generatePath _ (d, _) = error $ "Received invalid direction : " ++ [d]

walk :: Integral a => [(a, a)] -> (Char, a) -> [(a, a)]
walk path move = path ++ generatePath (last path) move

walkTheWay :: [(Char, Int)] -> [(Int, Int)]
walkTheWay = foldl walk [(0, 0)]

walkTheWayInSet :: [(Char, Int)] -> Data.Set.Set (Int, Int)
walkTheWayInSet = Data.Set.fromList . tail . walkTheWay

getIntersections :: [[(Char, Int)]] -> [(Int, Int)]
getIntersections (wa : wb : _) = Data.Set.toList
  (Data.Set.intersection (walkTheWayInSet wa) (walkTheWayInSet wb))

sumTuple :: Num a => (a, a) -> a
sumTuple (a, b) = abs a + abs b

findNearest :: [(Int, Int)] -> Int
findNearest = minimum . map sumTuple

solvePartOne :: String -> Int
solvePartOne = findNearest . getIntersections . parseInput

-- Part Two

distanceTo :: (Int, Int) -> [(Int, Int)] -> Int
distanceTo point path = case elemIndex point path of
  Nothing       -> error "point should exist in path"
  Just distance -> distance

sumOfDistance :: [(Int, Int)] -> [(Int, Int)] -> (Int, Int) -> Int
sumOfDistance pathOne pathTwo point =
  (distanceTo point pathOne) + (distanceTo point pathTwo)

solvePartTwo :: String -> Int
solvePartTwo input =
  minimum
    . map (sumOfDistance pathOne pathTwo)
    . getIntersections
    . parseInput
    $ input
  where (pathOne : pathTwo : _) = map walkTheWay . parseInput $ input

run = do
  input <-
    readFile
      "/Users/akhil/Desktop/Playground/advent-of-code/2019/advent-of-code-2019/resources/dayThree.txt"
  print $ solvePartOne input
