module Main where
import Data.Char
import Data.List
import Control.Parallel.Strategies

reactsWith :: Char -> Char -> Bool
reactsWith a b = match a b || match b a
  where match = \x y -> isLower x && isUpper y && x == toLower y

react :: String -> String
react (x:y:zs) =
  if x `reactsWith` y then
    react zs
  else
    x:react (y:zs)
react x = x

remove :: Char -> String -> String
remove c = filter (\x -> x /= c && x /= (toUpper c))

solve :: String -> String
solve input = show . minimum . parellel . fmap (length . until (\x -> react x == x) react . flip remove input) $ ['a'..'z']
  where parellel = withStrategy (parList rseq)

main = interact (solve . head . lines) >> putStr "\n"