{-# LANGUAGE TemplateHaskell #-}
module Main where
import Control.Lens
import Control.Monad.Trans.State
import Data.Set
import Data.Ix
import Data.Foldable
import Text.Parsec hiding (State)
import Text.Parsec.Char
import Text.Parsec.String


data Claim = Claim { _claimId :: Int, _x ::Int, _y ::Int, _w ::Int, _h::Int}
data PState = PState {_claimed :: Set (Int, Int), _overlapped :: Set (Int, Int)}

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


cell :: (Int, Int) -> Puzzle ()
cell c = do
  cl <- use claimed
  if member c cl then
    overlapped %= insert c
  else
    claimed %= insert c

step :: Claim -> Puzzle ()
step (Claim _ a b c d) = traverse_ cell (range ((a,b), (a+c-1, b+d-1)))

solve :: [Claim] -> Int
solve claims = evalState command (PState empty empty)
  where
    command = traverse_ step claims >> size <$> use overlapped

main = do
  c <- getContents
  case (parse (many1 (pClaim <* newline)) "input" c) of
    Left err -> putStrLn (show err)
    Right vals -> print $ solve vals