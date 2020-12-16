#!/usr/bin/env stack
-- stack --resolver lts-16.24 script

import Text.Parsec
import qualified Data.Set as S

parseGroup :: Parsec String u (S.Set Char)
parseGroup = foldr1 S.intersection
  <$> (S.fromList <$> many1 lower) `sepEndBy` endOfLine
  

main :: IO ()
main = do
  input <- getContents
  case parse (parseGroup `sepEndBy` endOfLine) "" input of
    Left err -> print err
    Right sets -> print $ sum $ fmap S.size sets
