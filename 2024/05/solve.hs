import Data.List (foldl')
import Data.Map hiding (drop, filter, foldl, foldl', map, mapMaybe, take)
import Data.Maybe (mapMaybe)
import Data.Text (lines, pack, splitOn, unpack)
import Debug.Trace (trace)
import Prelude hiding (lines, lookup)

main = interact $ show . (\input -> (solve1 input, solve2 input)) . parse

---------------- SHARED ----------------

type Orderings = Map Int [Int]

type Pages = [[Int]]

type Input = (Orderings, Pages)

parse :: String -> Input
parse input = do
  let [orderings', pages'] = splitOn (pack "\n\n") (pack input)
  let orderings :: Orderings =
        foldl'
          ( \acc line -> do
              let [from, to] = map (read . unpack) (splitOn (pack "|") line)
              insertWith (++) from [to] acc
          )
          mempty
          (lines orderings')
  let pages :: Pages = map (map (read . unpack) . splitOn (pack ",")) (lines pages')
  trace (show (orderings, pages)) (orderings, pages)

middle :: [a] -> a
middle xs = xs !! (length xs `div` 2)

dbg :: (Show a) => a -> a
dbg x = trace (show x) x

---------------- SOLUTION 1 ----------------

solve1 :: Input -> Int
solve1 (orders, pages) = sum $ map middle $ filter check pages
  where
    check :: [Int] -> Bool
    check [] = True
    check [x] = True
    check (from : rest) = case lookup from orders of
      Just to -> head rest `elem` to && check rest
      Nothing -> False

