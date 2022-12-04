module Main where

import Control.Exception (assert)
import Data.List.Split (splitOn)


-- Day 4: Camp Cleanup
main :: IO ()
main = do
    input <- readFile "input.txt"
    sample <- readFile "sample.txt"
    let part1 = assert (solve1 sample == 2) solve1 input
    let part2 = assert (solve2 sample == 4) solve2 input
    print (part1, part2)


solve1 :: String -> Integer
solve1 = toInteger . length . filter (uncurry contained) . map (pair . map section . splitOn ",") . lines

solve2 :: String -> Integer
solve2 = toInteger . length . filter (uncurry overlapped) . map (pair . map section . splitOn ",") . lines

contained :: Section -> Section -> Bool
contained (xl, xr) (yl, yr)
    | xl <= yl && yr <= xr = True
    | yl <= xl && xr <= yr = True
    | otherwise = False

overlapped :: Section -> Section -> Bool
overlapped (xl, xr) (yl, yr)
    | xl <= yl && yl <= xr = True
    | yl <= xl && xl <= yr = True
    | otherwise = False

section :: String -> Section
section = pair . map readInteger . splitOn "-"

type Section = (Integer, Integer)

pair :: [a] -> (a, a)
pair xs = assert (length xs == 2) (head xs, last xs)

readInteger :: String -> Integer
readInteger = read
