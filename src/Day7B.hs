module Main where
import Data.Graph.Inductive
import Data.Maybe
import Data.List
import Control.Lens
import Control.Monad.Loops (untilM_)
import Control.Monad.Trans.State
import Debug.Trace

parse :: [String] -> Gr Char ()
parse content = run_ (mkGraph (zip [1..] ['A'..'Z']) []) $
    traverse (insMapEdgeM . parseOne) content
  where parseOne x = (x !! 5, x !! 36, ())

solve :: String -> Int
solve input = (execState execM initial)^._3
  where
    gr = parse . filter ((> 0) . length) . lines $ input
    initial = (gr, [], -1)

step :: State (Gr Char (), [(Int, Int)], Int) ()
step = do
  _3 += 1
  _2.each._2 -= 1
  (done, doing) <- partition ((<= 0) . snd) <$> use _2
  gr <- _1 <%= delNodes (fmap fst done)
  let
    next = 5 - (length doing)
    contextFilter (a,n,_,_) = a == [] && not (n `elem` (fmap fst doing))
    nopreds = take next . sort . filter (contextFilter . fromJust . fst . flip match gr) $ nodes gr
  _2 .= doing ++ fmap (\x -> (x, x+60)) nopreds

execM = untilM_ step ((== empty) <$> use _1)


main :: IO ()
main = getContents >>= (print . solve)
