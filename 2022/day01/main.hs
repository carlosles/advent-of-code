import Data.List.Split (splitWhen)


-- Day 1: Calorie Counting
main :: IO ()
main = print . solution1 =<< readFile "day01/input.txt"

solution1 :: String -> Integer
solution1 = maximum . map (sum . map readInteger) . paragraphs

paragraphs :: String -> [[String]]
paragraphs = splitWhen null . lines

readInteger :: String -> Integer
readInteger = read
