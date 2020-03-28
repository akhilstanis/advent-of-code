module DayThree
  ( run
  )
where

import           Data.List.Split
import qualified Data.Set

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

walkTheWay :: [(Char, Int)] -> Data.Set.Set (Int, Int)
walkTheWay = Data.Set.fromList . tail . foldl walk [(0, 0)]

getIntersections :: [[(Char, Int)]] -> Data.Set.Set (Int, Int)
getIntersections (wa : wb : _) =
  Data.Set.intersection (walkTheWay wa) (walkTheWay wb)

sumTuple :: Num a => (a, a) -> a
sumTuple (a, b) = abs a + abs b

findNearest :: Data.Set.Set (Int, Int) -> Int
findNearest = Data.Set.findMin . Data.Set.map sumTuple

solve :: String -> Int
solve = findNearest . getIntersections . parseInput

run = do
  input <-
    readFile
      "/Users/akhil/Desktop/Playground/advent-of-code/2019/advent-of-code-2019/resources/dayThree.txt"
  print $ solve input