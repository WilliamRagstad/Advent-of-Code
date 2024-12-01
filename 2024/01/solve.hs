import Data.Bifunctor (bimap)
import Data.List (sort)

main = interact $ show . (\input -> (solve1 input, solve2 input)) . parseInput

parseInput :: String -> ([Int], [Int])
parseInput =
  foldr
    ( accumulate
        . map read
        . words
    )
    ([], [])
    . lines
  where
    accumulate [x, y] (xs, ys) = (x : xs, y : ys)

---------------- SOLUTION 1 ----------------
-- Sort two lists and taking the absolute difference
-- of the two integers, summing the differences.

solve1 :: ([Int], [Int]) -> Int
solve1 = sum . map abs . uncurry (zipWith (-)) . bimap sort sort

---------------- SOLUTION 2 ----------------
-- The sum of each number multiplied by the
-- number of times it appears in the second list.

solve2 :: ([Int], [Int]) -> Int
solve2 (xs, ys) = sum $ map (\x -> x * count x ys) xs
  where
    count x = length . filter (== x)
