import Data.Bifunctor (bimap)
import Data.List (sort)
import Data.List.NonEmpty (inits, tails)

main = interact $ show . (\input -> (solve1 input, solve2 input))

mul :: [Char] -> Maybe (Int, Int, [Char])
mul xs = do
  let (lhs, xs') = span is_num xs
  if head xs' == ','
    then do
      let (rhs, xs'') = span is_num (tail xs')
      if head xs'' == ')'
        then do
          let lhs' :: Int = read lhs
          let rhs' :: Int = read rhs
          Just (lhs', rhs', xs'')
        else Nothing
    else Nothing
  where
    is_num :: Char -> Bool
    is_num = (`elem` "0123456789")

---------------- SOLUTION 1 ----------------

solve1 :: [Char] -> Int
solve1 = solve1' 0
  where
    solve1' :: Int -> [Char] -> Int
    solve1' a "" = a -- base case
    solve1' a ('m' : 'u' : 'l' : '(' : xs) = case mul xs of
      Just (lhs, rhs, xs) -> solve1' (a + (lhs * rhs)) xs
      Nothing -> solve1' a xs -- ignore
    solve1' a (_ : xs) = solve1' a xs -- ignore

---------------- SOLUTION 2 ----------------

solve2 :: [Char] -> Int
solve2 = solve2' 0 True
  where
    solve2' :: Int -> Bool -> [Char] -> Int
    solve2' a _ "" = a -- base case
    solve2' a True ('m' : 'u' : 'l' : '(' : xs) = do
      case mul xs of
        Just (lhs, rhs, xs') -> solve2' (a + (lhs * rhs)) True xs'
        Nothing -> solve2' a True xs -- ignore
    solve2' a _ ('d' : 'o' : 'n' : '\'' : 't' : '(' : ')' : xs) = solve2' a False xs -- do()
    solve2' a _ ('d' : 'o' : '(' : ')' : xs) = solve2' a True xs -- don't()
    solve2' a cond (_ : xs) = solve2' a cond xs -- ignore
