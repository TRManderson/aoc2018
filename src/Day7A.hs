module Main where
import Data.Graph.Inductive
import Data.Maybe
import Data.List
import Data.Ord (comparing)

parse :: [String] -> Gr Char ()
parse content = run_ empty $
    insMapNodesM ['A'..'Z'] >> traverse (insMapEdgeM . parseOne) content
  where parseOne x = (x !! 5, x !! 36, ())

toposort :: Gr Char () -> [Char]
toposort gr = if gr == empty then [] else (snd next):rest
  where
    ns = labNodes gr
    nopreds = filter (\(n,_) -> (\(a,_,_,_) -> a == []) . fromJust . fst . match n $ gr) ns
    next = minimumBy (comparing snd) $ nopreds
    rest = toposort $ delNode (fst next) gr

solve :: String -> String
solve = toposort . parse . filter ((> 0) . length) . lines

main :: IO ()
main = getContents >>= (print . solve)
