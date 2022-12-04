import Control.Exception (assert)
import Data.List (sort)
import Data.List.Split (splitWhen)
import System.Posix (LockRequest(ReadLock))
import CoreSyn (OutAlt)


-- Day 2: Rock Paper Scissors
main :: IO ()
main = do
    input <- readFile "input.txt"
    sample <- readFile "sample.txt"
    let part1 = assert (solve1 sample == 15) solve1 input
    let part2 = assert (solve2 sample == 200) solve2 input
    print (part1, part2)


solve1 :: String -> Integer
solve1 = sum . map (score . play) . lines 

solve2 :: String -> Integer
solve2 x = 200

play :: String -> (Shape, Shape)
play s = (opponentShape (head s), ownShape (last s))

data Shape = Rock | Paper | Scissors deriving Eq

ownShape :: Char -> Shape
ownShape 'X' = Rock
ownShape 'Y' = Paper
ownShape 'Z' = Scissors
ownShape _ = undefined

opponentShape :: Char -> Shape
opponentShape 'A' = Rock
opponentShape 'B' = Paper
opponentShape 'C' = Scissors
opponentShape _ = undefined

score :: (Shape, Shape) -> Integer
score (opp, own) = outcomeScore (outcome own opp) + shapeScore own

outcome :: Shape -> Shape -> Outcome
outcome Rock Scissors = Win
outcome Paper Rock = Win
outcome Scissors Paper = Win
outcome x y | x == y = Draw
            | otherwise = Loss

data Outcome = Win | Draw | Loss

outcomeScore :: Outcome -> Integer
outcomeScore Win = 6
outcomeScore Draw = 3
outcomeScore Loss = 0

shapeScore :: Shape -> Integer
shapeScore Rock = 1
shapeScore Paper = 2
shapeScore Scissors = 3
