{-# LANGUAGE TemplateHaskell #-}
module Main where
import Control.Lens
import Control.Monad.Trans.State
import Data.IntSet as S
import Data.Map as M
import Data.Ix
import Data.Foldable hiding (toList)
import Text.Parsec hiding (State)
import Text.Parsec.Char
import Text.Parsec.String


data Claim = Claim { _claimId :: Int, _x ::Int, _y ::Int, _w ::Int, _h::Int}
data PState = PState {_claimed :: Map (Int, Int) Int, _unoverlapped :: IntSet}

makeLenses ''Claim
makeLenses ''PState

pClaim :: Parser Claim
pClaim = let pnum = read <$> many1 digit in Claim
  <$> (char '#' >> pnum)
  <*> (space >> char '@' >> space >> pnum)
  <*> (char ',' >> pnum)
  <*> (char ':' >> space >> pnum)
  <*> (char 'x' >> pnum)

type Puzzle a = State PState a


cell :: Int -> (Int, Int) -> Puzzle ()
cell idx c = do
  cl <- use claimed
  case M.lookup c cl of
    Just idx' -> do
      unoverlapped %= S.delete idx
      unoverlapped %= S.delete idx'
    Nothing -> claimed %= M.insert c idx

solve :: [Claim] -> IntSet
solve claims = evalState command (PState M.empty S.empty)
  where
    step (Claim i a b c d) = traverse_ (cell i) (range ((a,b), (a+c-1, b+d-1)))
    command = do
      traverse (\(Claim x _ _ _ _) -> unoverlapped %= S.insert x) claims
      traverse_ step claims
      use unoverlapped

main = do
  c <- getContents
  case (parse (many1 (pClaim <* newline)) "input" c) of
    Left err -> putStrLn (show err)
    Right vals -> print $ solve vals