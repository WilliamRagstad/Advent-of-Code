-- https://adventofcode.com/2015/day/1#part2

main :: IO ()
main = do
  input <- readFile "input.txt"
  print $ task input

-- Task functions

task :: [Char] -> Int
task = tfloor 1 0 -- The first character in the instructions has position 1 and he starts on the ground floor (floor 0).

-- tfloor :: (Eq a1, Num a1, Num a2) => [Char] -> a2 -> a1 -> a2
tfloor :: (Eq a1, Num a1, Num a2) => a2 -> a1 -> [Char] -> a2
tfloor p a (')' : xs) =
  case a - 1 of
    -1 -> p
    v -> tfloor (p + 1) v xs
tfloor p a (_ : xs) = tfloor (p + 1) (a + 1) xs
tfloor _ _ [] = -1