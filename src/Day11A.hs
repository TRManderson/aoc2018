module Main where
import Data.Array
import Data.List
import Data.Ord (comparing)
import Control.Parallel.Strategies

build :: Ix i => (i, i) -> (i -> e) -> Array i e
build ixs f = listArray ixs (withStrategy (parList rseq) . fmap f . range $ ixs)

value x y = ((basePower `rem` 1000) `div` 100) - 5
  where
    rackID = x + 10
    serial = 1133 -- puzzle input
    basePower = (rackID * y + serial) * rackID


powerLevels = build ((1,1), (300, 300)) (uncurry value)


cellVal (a,b) = sum . fmap (powerLevels !) . range $ ((a,b), (a+2, b+2))


largestLot = maximumBy (comparing cellVal) $ range ((1,1), (300-2, 300-2))


main = print largestLot