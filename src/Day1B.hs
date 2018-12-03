module Main where
import Data.IntSet

reducer :: [Int] -> Int
reducer = go (empty, 0)
  where
    go :: (IntSet, Int) -> [Int] -> Int
    go (s, v) (x:xs) = if member v s then v else go (insert v s, v + x) xs 

dropPlus :: String -> String
dropPlus ('+':xs) = xs
dropPlus rest = rest

solve :: String -> String
solve = show . reducer . cycle . fmap (read . dropPlus) . lines


main :: IO ()
main = interact solve >> putStr "\n"
