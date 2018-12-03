module Main where

dropPlus :: String -> String
dropPlus ('+':xs) = xs
dropPlus rest = rest

solve :: String -> String
solve = show . sum . fmap (read . dropPlus) . lines

main :: IO ()
main = interact solve >> putStr "\n"
