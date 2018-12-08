module Main where
import Text.Megaparsec
import Text.Megaparsec.Char
import Control.Applicative ((*>))

readNum :: (Ord e) => Parsec e String Int
readNum = read <$> some digitChar

pNode :: Parsec Int String Int
pNode = do
  children <- readNum
  space
  metadata <- readNum
  childMeta <- sum <$> count children (space *> pNode)
  metas <- sum <$> count metadata (space *> readNum)
  pure (metas + childMeta)


main = getContents >>= (print . (\(Right x) -> x) . parse pNode "stdin" . head . lines)