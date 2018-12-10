{-# LANGUAGE TemplateHaskell #-}
module Main where
import Control.Lens
import Control.Monad.Trans.State
import Data.Set
import Data.Foldable
import Control.Monad.Loops (whileM)
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char


data Point = Point {
  _x ::Int, _y::Int, _vx::Int, _vy::Int
}
makeLenses ''Point

step :: State ([Point], Int) String
step = do
  _2 += 1
  _1.each %= \(Point x y vx vy) -> (Point (x+vx) (y+vy) vx vy)
  maxX <- maximum . fmap _x <$> use _1
  maxY <- maximum . fmap _y <$> use _1
  minX <- minimum . fmap _x <$> use _1
  minY <- minimum . fmap _y <$> use _1
  pointSet <- fromList . fmap (\(Point x y _ _) -> (x,y)) <$> use _1
  v <- use _2
  if (maxX - minX) < 100 && (maxY - minY) < 100 then
    pure . (("\n" ++ (show v) ++ "\n") ++) $ flip concatMap [minY..maxY] $ \y ->
      flip concatMap [minX..maxX] (\x ->
            if (x, y) `member` pointSet then
              ['#']
            else ['.']) ++ ['\n']
  else
    pure ""


solve ls = traverse putStr $ evalState (whileM (pure True) step) (ls, 0)

parse_ = fmap ((\(Right x) -> x) . parse parser "stdin") . lines
  where
    parseNum = do
       (v:xs) <- (:) <$> oneOf " -" <*> some digitChar
       pure $ if v == ' ' then read xs else read (v:xs)
    parser :: Parsec () String Point
    parser = do
      string"position=<"
      x <- parseNum
      string ", "
      y <- parseNum
      string "> velocity=<"
      vx <- parseNum
      string ", "
      vy <- parseNum
      char '>'
      pure $ Point x y vx vy



main = getContents >>= (solve . parse_)