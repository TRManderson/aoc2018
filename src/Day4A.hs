{-# LANGUAGE Rank2Types #-}
module Main where
import Control.Lens
import Control.Monad.Trans.State
import Data.Array
import Data.Map hiding (update, assocs, elems)
import Data.List (sort, maximumBy)
import Data.Ord (comparing)
import Data.Either (fromRight)
import Text.Megaparsec hiding (State, empty)
import Text.Megaparsec.Char

type PMap = Map Int (Array Int Int)
type PState = (PMap, Int, Int)
data Entry = Guard Int | Sleep Int | Wake Int deriving (Ord, Eq)

emptyArr = listArray (0, 59) $ replicate 60 0

add :: Int -> (Int, Int) -> State PState ()
add g ixs = do
  v <- use $ _1.at g
  case v of
    Just (arr) -> do
      traverse (\i -> (_1. ix g . ix i) += 1) $ range ixs
      pure ()
    Nothing -> do
      _1.at g .= Just emptyArr
      add g ixs

update :: Entry -> State PState ()
update (Guard x) = _2 .= x
update (Sleep x) = _3 .= x
update (Wake x) = do
  s <- use _3
  g <- use _2
  add g (s, x-1)


type Parser = Parsec () String Entry

parse_ :: String -> Entry
parse_ = (\(Right x) -> x) . parse parser "stdin"
  where
    parser :: Parser
    pGuard :: Int -> Parser
    pWake :: Int -> Parser
    pSleep :: Int -> Parser
    pGuard _ = do
      string "Guard #"
      val <- read <$> some digitChar
      string " begins shift"
      pure (Guard val)
    pSleep v = string "falls asleep" >> pure (Sleep v)
    pWake v = string "wakes up" >> pure (Wake v)
    parser = do
      skipCount 15 asciiChar
      val <- (read::String -> Int) <$> some digitChar
      char ']'
      space
      choice . fmap ($ val) $ [pGuard, pSleep, pWake]

solve :: String -> PState
solve = flip execState (empty, 0, 0) . traverse (update . parse_) . sort . lines
getAnswer :: PState -> Int
getAnswer (x, _, _) =  val * minute
  where
    (val, highestGuard) = maximumBy (comparing $ sum . elems . snd) $ toList x
    minute = fst . maximumBy (comparing snd) $ assocs highestGuard


main = getContents >>= (print . getAnswer . solve)