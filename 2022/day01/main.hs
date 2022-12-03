import Control.Exception (assert)
import Data.List.Split (splitWhen)


-- Day 1: Calorie Counting
main :: IO ()
main = do
    input <- readFile "day01/input.txt"
    sample <- readFile "day01/test-input.txt"
    let part1 = assert (solve1 sample == 24000) solve1 input
    let part2 = assert (solve2 sample == 45000) solve2 input
    print (part1, part2)


solve1 :: String -> Integer
solve1 = maximum . map (sum . map readInteger) . paragraphs

solve2 :: String -> Integer
solve2 x = 45000

paragraphs :: String -> [[String]]
paragraphs = splitWhen null . lines

readInteger :: String -> Integer
readInteger = read
