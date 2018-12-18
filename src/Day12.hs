module Main where
import Data.Maybe (fromJust)
import Data.List (genericTake)
import Data.List.PointedList hiding (length)

nextGen (True, False, True, False, True) = True
nextGen (False, True, True, False, False) = False
nextGen (True, False, True, False, False) = False
nextGen (False, False, True, True, True) = True
nextGen (False, True, False, False, True) = True
nextGen (False, False, True, False, False) = False
nextGen (True, True, True, True, False) = True
nextGen (True, True, True, False, False) = True
nextGen (True, False, False, False, False) = False
nextGen (False, True, False, True, False) = True
nextGen (False, False, False, False, True) = False
nextGen (True, False, False, False, True) = True
nextGen (False, False, True, False, True) = True
nextGen (True, False, False, True, False) = True
nextGen (False, True, False, False, False) = True
nextGen (True, True, False, False, True) = False
nextGen (True, True, False, False, False) = False
nextGen (True, False, False, True, True) = False
nextGen (False, True, False, True, True) = True
nextGen (False, True, True, False, True) = False
nextGen (True, False, True, True, False) = True
nextGen (False, True, True, True, True) = False
nextGen (False, True, True, True, False) = False
nextGen (False, False, True, True, False) = False
nextGen (True, True, False, True, False) = False
nextGen (False, False, False, True, True) = True
nextGen (False, False, False, True, False) = False
nextGen (False, False, False, False, False) = False
nextGen (True, True, False, True, True) = False
nextGen (True, True, True, False, True) = True
nextGen (True, True, True, True, True) = True
nextGen (True, False, True, True, True) = False

step (PointedList ((_, b):(_, a):_) (x, c) ((_, d):(_, e):_)) = (x, nextGen (a,b,c,d,e))

presetState = [True, False, True, True, True, False, False, False, False, False, False, False, True, False, False, True, False, True, True, False, False, True, True, True, True, True, False, False, False, True, False, False, False, True, True, True, True, True, True, True, False, False, False, False, True, True, False, True, True, False, True, True, False, True, True, False, False, True, False, True, False, True, True, True, True, True, True, True, True, True, True, False, False, False, True, True, False, True, True, False, False, True, True, False, True, True, False, False, False, True, True, True, True, False, False, True, True, True, True]
initial = PointedList (zip [-1, -2..] $ repeat False) (0, True) (zip [1..] $  presetState ++ repeat False)


getContext = take 2300 . _suffix . fromJust . moveN (-1000)

-- for part B it becomes linear
main = traverse print . take 200 . zip [0..] . fmap (sum . fmap fst . filter snd . getContext) . iterate (contextMap step) $ initial