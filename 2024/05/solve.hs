import Data.Foldable (maximumBy)
import Data.List (foldl')
import Data.Map hiding (drop, filter, foldl, foldl', map, mapMaybe, take)
import Data.Maybe (mapMaybe)
import Data.Ord (comparing)
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
  dbg "Parsed: " (orderings, pages)

middle :: [a] -> a
middle xs = xs !! (length xs `div` 2)

dbg :: (Show a) => String -> a -> a
dbg s x = trace (s ++ show x) x

isSorted :: Orderings -> [Int] -> Bool
isSorted _ [] = True
isSorted _ [x] = True
isSorted order (from : rest) = case lookup from order of
  Just to -> head rest `elem` to && isSorted order rest
  Nothing -> False

---------------- SOLUTION 1 ----------------

solve1 :: Input -> Int
solve1 (order, pages) = sum $ map middle $ filter (isSorted order) pages

---------------- SOLUTION 2 ----------------
-- Add up their middle page numbers, after taking only
-- the incorrectly-ordered updates and ordering them correctly.

solve2 :: Input -> Int
solve2 (order, pages) = sum $ map (middle . dbg "Fixed: " . fix) $ filter (not . isSorted order) pages
  where
    fix :: [Int] -> [Int]
    fix [] = []
    fix [x] = [x]
    -- fix (from : rest) = case lookup from order of
    --   Just to -> case filter (`elem` to) rest of
    --     [] -> from : fix rest
    --     x : _ -> x : fix rest
    --   Nothing -> from : fix rest
    -- fix (from : rest) = case lookup from order of
    --   Just tos ->
    --     -- if head rest `elem` to
    --     --   then
    --     --     from : fix rest
    --     --   else

    --     -- Find the first next correct element in rest
    --     -- that are a valid order from the current element
    --     -- and swap it with the head of rest (if any)
    --     -- then contine fixing the rest of the list
    --     let (idx, next) = head $ filterIdx (`elem` tos) rest
    --      in -- Remove the element at the idx
    --         let rest' = take idx rest ++ drop (idx + 1) rest
    --          in from : next : fix rest'
    --   Nothing -> from : fix rest
    fix all = do
      -- The last element is the only one that does not have any order
      -- edge present in the element list
      let start = dbg "Start: " $ findStart order (dbg "\n\nFixing: " all)
      -- Select the preceding element that has an order edge to the last element
      -- case prependFrom order all last of
      --   Just all' -> reverse $ last : all'
      --   Nothing -> error "No solution found"
      -- Remove first `start` from all
      let (_, rest) = extractFirst (== start) all
      case sort order rest [start] of
        [] -> error "No solution found"
        -- If the orders are commutative or distributive,
        -- then we can have multiple solutions. Take the longest one.
        results -> maximumBy (comparing length) results
    --  in reverse $ start' : head (dbg "Pre-rev: " ordered)

    -- Find the element that does not have any order edge by
    -- Removing each elements that has an order edge to it
    -- until only one element remains
    findStart :: Orderings -> [Int] -> Int
    findStart order elems =
      case filterElems order [] elems of
        [start] -> start
        [] -> error "No start found"
        starts -> error ("Multiple starts found: " ++ show starts)
      where
        -- Remove all elements that has an order edge to any of the `elems`
        filterElems :: Orderings -> [Int] -> [Int] -> [Int]
        filterElems order _ [] = []
        filterElems order seen (x : xs) =
          -- Check if x has an order edge to any of the not yet checked elements
          -- or already removed elements has an order edge to x
          -- if hasOrderIn order (xs ++ removed) x
          if any (\to -> hasOrderTo order to x) (xs ++ seen)
            then filterElems order (x : seen) xs
            else x : filterElems order (x : seen) xs
    -- Check if a `from` elem has an order edge to any of the `elems`
    -- hasOrderIn :: Orderings -> [Int] -> Int -> Bool
    -- hasOrderIn order elems to = case lookup to order of
    --   Just tos -> any (`elem` tos) elems
    --   Nothing -> False

    sort :: Orderings -> [Int] -> [Int] -> [[Int]]
    sort _ _ [] = error "No start found"
    sort _ [] result = [result]
    sort order remaining (to : result) = do
      -- case lookup to order of
      --   Just tos ->
      --     let (from, rest) = extractFirst (`elem` tos) remaining
      --      in reOrder order rest (from : to : result)
      --   Nothing -> error "No order found"
      -- Explore search space of all remaining that has an order edge to `to`
      let froms = filter (hasOrderTo order to) remaining
      case froms of
        [] ->
          -- If no elements has an order edge to `to`
          -- and `remaining` is not empty, then we have a problem.
          -- This means that the current path is invalid and we need to backtrack.
          -- Return an empty list to indicate that the current path is invalid.
          dbg ("To: " ++ show to ++ " From: ") []
        -- [to : result]
        _ -> do
          let paths =
                foldl'
                  ( \acc from -> do
                      let (_, rest) = extractFirst (== from) remaining
                      let result' = from : to : result
                      sort order rest (dbg ("Remaining: " ++ show rest ++ " Searching: ") result') ++ acc
                  )
                  []
                  (dbg ("To: " ++ show to ++ " From: ") froms)
          dbg "Paths: " paths

    -- prependFrom :: Orderings -> [Int] -> Int -> [[Int]]
    -- prependFrom _ [] _ = []
    -- prependFrom order remaining to =
    --   let froms = filter (hasOrderTo order to) remaining
    --    in foldl
    --         ( \acc from ->
    --             let (_, rest) = extractFirst (== from) remaining
    --              in let options = prependFrom order rest from
    --                  in case options of
    --                       [] -> []
    --                       (next : _) -> next : acc -- [from, next] ++ rest
    --         )
    --         []
    --         froms
    extractFirst :: (a -> Bool) -> [a] -> (a, [a])
    extractFirst f xs = case break f xs of
      (before, x : after) -> (x, before ++ after)
      _ -> error "No element found"

    hasOrderTo :: Orderings -> Int -> Int -> Bool
    hasOrderTo order to from = case lookup from order of
      Just tos -> to `elem` tos
      Nothing -> False

--  in case lookup to order of
--       Just tos ->
--         -- select an element from the remaining list that is in the tos list
--         let options = filterIdx (`elem` tos) remaining
--          in case trace (show (options, remaining, to)) options of
--               [] -> remaining
--               (idx, next) : _ ->
--                 -- Find the first next correct element in rest
--                 -- that are a valid order from the current element
--                 -- and swap it with the head of rest (if any)
--                 -- then contine fixing the rest of the list
--                 let remaining' = take idx remaining ++ drop (idx + 1) remaining
--                  in prependFrom order remaining' next ++ [next]
--       Nothing -> remaining

-- filterIdx :: (a -> Bool) -> [a] -> [(Int, a)]
-- filterIdx f xs = filter (f . snd) (zip [0 ..] xs)
