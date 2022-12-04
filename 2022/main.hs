module Main where

import Control.Exception (assert)


-- Day XX: Title
main :: IO ()
main = do
    input <- readFile "input.txt"
    sample <- readFile "sample.txt"
    let part1 = assert (solve1 sample == 100) solve1 input
    let part2 = assert (solve2 sample == 200) solve2 input
    print (part1, part2)


solve1 :: String -> Integer
solve1 x = 100

solve2 :: String -> Integer
solve2 x = 200
