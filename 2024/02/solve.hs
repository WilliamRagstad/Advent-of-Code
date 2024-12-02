import Data.Bifunctor (bimap)
import Data.List (sort)
import Data.List.NonEmpty (inits, tails)

main = interact $ show . (\input -> (solve1 input, solve2 input)) . parseInput

parseInput :: String -> [[Int]]
parseInput s = map (map read . words) (lines s)

---------------- SOLUTION 1 ----------------
-- Count the number of reports with levels changing
-- by 1 or 2, AND (strictly increasing or decreasing)

solve1 :: [[Int]] -> Int
solve1 input = do
  length $ filter safe input

safe :: [Int] -> Bool
safe report =
  (increasing report || decreasing report) && maxChange report

increasing :: [Int] -> Bool
increasing levels =
  all (uncurry (<)) (zip levels (tail levels))

decreasing :: [Int] -> Bool
decreasing levels =
  all (uncurry (>)) (zip levels (tail levels))

maxChange :: [Int] -> Bool
maxChange report =
  all ((`elem` [1, 2, 3]) . abs) (zipWith (-) report (tail report))

---------------- SOLUTION 2 ----------------
-- Same as above, but try removing one report

solve2 :: [[Int]] -> Int
solve2 input = do
  length $ filter (any safe . missing) input
  where
    missing :: [Int] -> [[Int]]
    missing report = [take i report ++ drop (i + 1) report | i <- [0 .. length report - 1]]
