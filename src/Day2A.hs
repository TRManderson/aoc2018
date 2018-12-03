module Main where
import Data.Map hiding (fold, foldr)
import Data.Foldable (fold)
import Data.Monoid (Sum(..))


check :: String -> (Sum Int, Sum Int)
check = result . elems . foldr transition empty
  where
    transition v = insertWith (+) v 1
    f v = Sum . fromEnum . any (== v)
    result vals = (f 2 vals, f 3 vals)

solve :: String -> String
solve = show . (\(Sum x, Sum y) -> x*y) . fold . fmap check . lines

main = interact solve >> putStr "\n"