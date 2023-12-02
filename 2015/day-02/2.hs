-- https://adventofcode.com/2015/day/2

main :: IO ()
main = do
  input <- readFile "input.txt"
  print $ task input

-- Helper functions

split :: Char -> String -> [String]
split _ [] = [""]
split delim (c : cs)
  | c == delim = "" : rest
  | otherwise = (c : head rest) : tail rest
  where
    rest = split delim cs

-- Task functions

task :: String -> Int
task = foldr sArea 0 . presents

presents :: String -> [(Int, Int, Int)]
presents = map present . lines

present :: String -> (Int, Int, Int)
present p = toTuple3 . map toInt $ split 'x' p

toInt :: String -> Int
toInt = read

toTuple3 :: [Int] -> (Int, Int, Int)
toTuple3 [x, y, z] = (x, y, z)
toTuple3 _ = error "All presents must have three dimensional lengths!"

sArea :: (Int, Int, Int) -> Int -> Int
sArea (x, y, z) a = a + 2 * (x * y + y * z + z * x) + min3 (x * y) (y * z) (z * x) -- increment the accumulator a with the new present surface area

min3 :: Int -> Int -> Int -> Int
min3 x y = min (min x y)