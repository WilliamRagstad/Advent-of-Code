main = interact $ show . (\input -> (solve1 input, solve2 input)) . lines

---------------- SHARED ----------------

sublists :: String -> [String]
sublists [] = []
sublists xs@(_ : xs') = xs : sublists xs'

countSubstrs :: String -> Int -> String -> Int
countSubstrs what dist str = length $ filter (isSubstr what dist) $ sublists str
  where
    isSubstr :: String -> Int -> String -> Bool
    isSubstr _ _ "" = False -- No str left to search in
    isSubstr "" _ _ = True -- Found end of 'what' :D
    isSubstr (what_c : what_xs) dist (str_c : str_xs) =
      -- If current match expected, continue
      (what_c == str_c) && isSubstr what_xs dist (drop dist str_xs)

---------------- SOLUTION 1 ----------------
-- Count "XMAS" horizontal, vertical,
-- diagonal and backwards separately.

solve1 :: [String] -> Int
solve1 input = do
  let width = length (head input) - 1 -- ignore \n
  let one_input = concat input
  let rev_input = reverse one_input
  let countXmas = countSubstrs "XMAS"
  countXmas 0 one_input -- horizontal right
    + countXmas 0 rev_input -- horizontal left
    + countXmas width one_input -- vertical down
    + countXmas width rev_input -- vertical up
    + countXmas (width - 1) one_input -- diagonal down left
    + countXmas (width - 1) rev_input -- diagonal up right
    + countXmas (width + 1) one_input -- diagonal down right
    + countXmas (width + 1) rev_input -- diagonal up left

-- 2452 too low

---------------- SOLUTION 2 ----------------

solve2 :: [String] -> Int
solve2 input = -1
