module Main where
import Data.Array
import Control.Comonad
import Control.Parallel.Strategies


data Pointer i e = P { pointerIdx :: i, underlying :: (Array i e) } deriving Show

instance Ix i => Functor (Pointer i) where
  fmap f (P i a) = P i (fmap f a)

instance Ix i => Comonad (Pointer i) where
  extract (P i a) = a ! i
  extend f (P i a) = P i $ listArray bds (parMap rseq (f . flip P a) (range bds))
    where bds = bounds a

data Point = Tree | Lumber | Open deriving (Show, Eq)

parse '.' = Open
parse '#' = Lumber
parse '|' = Tree
parse c = error $ "Unknown character in input: " ++ show c


boundingPair = ((1,1), (50,50))

fromInput content = P (1,1) . array boundingPair $ do
  (y, cols) <- zip [1..] $ lines content
  (x, cell) <- zip [1..] cols
  pure $ ((x,y), parse cell)

adjacents :: Pointer (Int, Int) Point -> [Point]
adjacents (P (x, y) arr) = fmap (arr !) . filter (inRange (bounds arr)) $
  [ (x + 1, y + 1)
  , (x + 1, y - 1)
  , (x - 1, y + 1)
  , (x - 1, y - 1)
  , (x, y + 1)
  , (x, y - 1)
  , (x + 1, y)
  , (x - 1, y)
  ]

transition :: Pointer (Int, Int) Point -> Point
transition p = let adjs = adjacents p in case extract p of
    Open -> if (length . filter (== Tree) $ adjs) >= 3 then Tree else Open
    Tree -> if (length . filter (== Lumber) $ adjs) >= 3 then Lumber else Tree
    Lumber -> if (Tree `elem` adjs && Lumber `elem` adjs) then Lumber else Open

value :: Ix i => Array i Point -> Int
value arr = (check Tree) * (check Lumber)
  where
    check x = length . filter (== x) . elems $ arr

-- like day 12 I expected a trend
-- trial and error showed 300 was not enough
-- ended up cycling between values every 28 iterations, plotted on excel
main = getContents >>= (traverse print . zip [0..] . (fmap $ value . underlying) . take 3000 . drop 10000 . iterate (extend transition) . fromInput)
