import Control.Exception (assert)
import Data.List (sort)
import Data.List.Split (splitWhen)


-- Day 2: Rock Paper Scissors
main :: IO ()
main = do
    input <- readFile "input.txt"
    sample <- readFile "sample.txt"
    let part1 = assert (solve1 sample == 15) solve1 input
    let part2 = assert (solve2 sample == 12) solve2 input
    print (part1, part2)


solve1 :: String -> Integer
solve1 = sum . map (score . play1 . instruction) . lines 

solve2 :: String -> Integer
solve2 = sum . map (score . play2 . instruction) . lines

instruction :: String -> (Char, Char)
instruction s = (head s, last s)

play1 :: (Char, Char) -> (Shape, Shape)
play1 (opp, own) = (shape opp, decrypt1 own)

play2 :: (Char, Char) -> (Shape, Shape)
play2 (opp, own) = (opp_shape, inverse target_outcome opp_shape)
    where
        opp_shape = shape opp
        target_outcome = decrypt2 own
 
data Shape = Rock | Paper | Scissors deriving Eq

decrypt1 :: Char -> Shape
decrypt1 'X' = Rock
decrypt1 'Y' = Paper
decrypt1 'Z' = Scissors
decrypt1 _ = undefined

decrypt2 :: Char -> Outcome
decrypt2 'X' = Loss
decrypt2 'Y' = Draw
decrypt2 'Z' = Win
decrypt2 _ = undefined

shape :: Char -> Shape
shape 'A' = Rock
shape 'B' = Paper
shape 'C' = Scissors
shape _ = undefined

score :: (Shape, Shape) -> Integer
score (opp, own) = outcomeScore (outcome own opp) + shapeScore own

outcome :: Shape -> Shape -> Outcome
outcome Rock Scissors = Win
outcome Paper Rock = Win
outcome Scissors Paper = Win
outcome x y | x == y = Draw
            | otherwise = Loss

inverse :: Outcome -> Shape -> Shape
inverse Win Rock = Paper
inverse Win Paper = Scissors
inverse Win Scissors = Rock
inverse Loss Rock = Scissors
inverse Loss Paper = Rock
inverse Loss Scissors = Paper
inverse Draw x = x

data Outcome = Win | Draw | Loss

outcomeScore :: Outcome -> Integer
outcomeScore Win = 6
outcomeScore Draw = 3
outcomeScore Loss = 0

shapeScore :: Shape -> Integer
shapeScore Rock = 1
shapeScore Paper = 2
shapeScore Scissors = 3
