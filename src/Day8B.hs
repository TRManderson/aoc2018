module Main where
import Text.Megaparsec
import Text.Megaparsec.Char
import Control.Applicative ((*>))

readNum :: (Ord e) => Parsec e String Int
readNum = read <$> some digitChar

data Node = Node [Node] [Int]
pNode :: Parsec Int String Node
pNode = do
  children <- readNum
  space
  metadata <- readNum
  childMeta <- count children (space *> pNode)
  metas <- count metadata (space *> readNum)
  pure (Node childMeta metas)


solve :: Node -> Int
solve (Node children metas) = 
  let
    numChildren = length children
    perMeta x = if x <= 0 || x > numChildren then 0 else (solve $ children !! (x-1))
  in
    if numChildren == 0 then
      sum metas
    else
      sum $ fmap perMeta metas




main = getContents >>= (print . solve . (\(Right x) -> x) . parse pNode "stdin" . head . lines)