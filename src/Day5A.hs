module Main where
import Data.Char

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

solve :: String -> String
solve = show . length . until (\x -> (react x) == x) react . head . lines

main = interact solve >> putStr "\n"