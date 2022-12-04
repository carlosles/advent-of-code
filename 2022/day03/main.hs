module Main where

import Control.Exception (assert)
import Data.Char (ord)
import Data.List (intersect)
import Data.List.Split (chunksOf)
import Data.Tuple (uncurry)


-- Day 3: Rucksack Reorganization
main :: IO ()
main = do
    input <- readFile "input.txt"
    sample <- readFile "sample.txt"
    let part1 = assert (solve1 sample == 157) solve1 input
    let part2 = assert (solve2 sample == 70) solve2 input
    print (part1, part2)


solve1 :: String -> Integer
solve1 = sum . map (priority . head . uncurry intersect . halve) . lines

solve2 :: String -> Integer
solve2 = sum . map (priority . badge) . chunksOf 3 . lines

priority :: Char -> Integer
priority c | c < 'a' = toInteger (1 + ord c - ord 'A') + priority 'z'
           | otherwise = toInteger (1 + ord c - ord 'a')

halve :: [a] -> ([a], [a])
halve xs = splitAt (length xs `div` 2) xs

badge :: [String] -> Char
badge cs = head $ foldr intersect (head cs) (tail cs)
