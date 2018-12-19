module Main where
import Data.Array
import Data.List
import Data.Ord (comparing)
import Control.Parallel.Strategies
import Data.Function.Memoize

mkpar :: NFData a => [a] -> [a]
mkpar = withStrategy (parList rdeepseq)

build :: (Ix i, NFData e) => (i, i) -> (i -> e) -> Array i e
build ixs f = listArray ixs (mkpar . fmap f . range $ ixs)

value x y = ((basePower `rem` 1000) `div` 100) - 5
  where
    rackID = x + 10
    serial = 1133 -- puzzle input
    basePower = (rackID * y + serial) * rackID


powerLevels = build ((1,1), (300, 300)) (uncurry value)


blockVal a b adj = sum . fmap (powerLevels !) . range $ ((a,b), (a+adj, b+adj))

largestBlock :: (Int, Int, Int)
largestBlock = snd . maximumBy (comparing fst) $ do
  adj <- [299, 298..0]
  pure . maximumBy (comparing fst) . mkpar $ do
    (x, y) <- range ((1,1), (300-adj, 300-adj))
    pure $ (blockVal x y adj, (x, y, adj+1))

main = print largestLot