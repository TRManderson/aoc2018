module Main where

-- feeling bad at the O(n^2)
solve :: String -> String
solve val = let lined = lines val in head $ do
  line1 <- lined
  let len = length line1
  line2 <- lined
  let res = filter snd . zip line1 $ zipWith (==) line1 line2
  if length res == (len - 1) then
    [map fst res]
  else
    []

main = interact solve >> putStr "\n"