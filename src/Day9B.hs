module Main where
import Data.List.PointedList.Circular
import Data.Array
import Control.Lens
import Control.Monad.Trans.State
import Data.Maybe (fromJust)
import Debug.Trace

-- input
playerCount = 476
lastMarble = 71657 * 100

turn :: Int -> Int -> State (Array Int Int, PointedList Int) ()
turn pl mar =
  if (mar `rem` 23) == 0 then do
    _2 %= moveN (-7)
    val <- use $ _2.focus
    _1.ix pl += mar + val
    _2 %= fromJust . delete
  else do
    _2 %= next
    _2 %= insertRight mar


toFold = sequence_ $ zipWith turn players marbles

marbles = [1..lastMarble]
players = cycle [1..playerCount]
initPlayerArr = listArray (1, playerCount) (replicate playerCount 0)

main = print . maximum . fst $ execState toFold (initPlayerArr, singleton 0)