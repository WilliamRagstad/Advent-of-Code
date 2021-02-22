-- https://adventofcode.com/2015/day/1

main :: IO ()
main = do
  input <- readFile "input.txt"
  print $ task input

-- Task functions

task :: [Char] -> Int
task = foldr tfloor 0

tfloor :: Num a => Char -> a -> a
tfloor '(' a = a + 1
tfloor ')' a = a - 1
tfloor _ a = a