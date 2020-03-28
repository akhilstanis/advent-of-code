module DayTwo
  ( run
  )
where

import           Data.List.Split
import           Data.List

type Compute = Int -> [Int] -> [Int]

compute :: Compute
compute pos program = case program !! pos of
  1  -> computeSum pos program
  2  -> computeProduct pos program
  99 -> program

computeSum = computeArithmetic (+)
computeProduct = computeArithmetic (*)

computeArithmetic operation pos program = compute newPos newProgram
 where
  operand1    = getOperand program (pos + 1)
  operand2    = getOperand program (pos + 2)
  destination = program !! (pos + 3)
  newProgram  = store destination (operation operand1 operand2) program
  newPos      = pos + 4

getOperand program pos = program !! address where address = program !! pos

store :: Int -> a -> [a] -> [a]
store pos val xs = take pos xs ++ [val] ++ drop (pos + 1) xs

computeFromStart = compute 0
restoreGravityAssist = store 1 12 . store 2 2

readInt :: String -> Int
readInt = read
readInput = map readInt . splitOn ","

restoreAndCompute = head . computeFromStart . restoreGravityAssist . readInput

-- Part 2

setNoundAndVerb noun verb = store 1 noun . store 2 verb

possibleNounAndVerbs = [ (i, j) | i <- [0 .. 99], j <- [0 .. 99] ]

restoreAndComputeWithNounAndVerb noun verb =
  head . computeFromStart . setNoundAndVerb noun verb . readInput

evaluatesTo input output (noun, verb) =
  restoreAndComputeWithNounAndVerb noun verb input == output

solution input = find (evaluatesTo input 19690720) possibleNounAndVerbs

run = do
  input <-
    readFile
      "/Users/akhil/Desktop/Playground/advent-of-code/2019/advent-of-code-2019/resources/dayTwo.txt"
  print $ solution input
