module Main where

import Control.Exception (assert)
import Data.List (sort)
import Data.List.Split (splitWhen)


-- Day 1: Calorie Counting
main :: IO ()
main = do
    input <- readFile "input.txt"
    sample <- readFile "sample.txt"
    let part1 = assert (solve1 sample == 24000) solve1 input
    let part2 = assert (solve2 sample == 45000) solve2 input
    print (part1, part2)


solve1 :: String -> Integer
solve1 = maximum . totalCalories

solve2 :: String -> Integer
solve2 = sum . take 3 . reverse . sort . totalCalories

totalCalories :: String -> [Integer]
totalCalories = map (sum . map readInteger) . paragraphs

paragraphs :: String -> [[String]]
paragraphs = splitWhen null . lines

readInteger :: String -> Integer
readInteger = read
