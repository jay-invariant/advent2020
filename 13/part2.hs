#!/usr/bin/env stack
-- stack --resolver lts-16.24 script --package parsec,besout --extra-dep besout-0.2.0.1

import Text.Parsec
import Data.Maybe
import Data.List
import Bezout

stopsList :: Parsec String u (Integer, [Maybe Integer])
stopsList = do
  start <- read <$> many1 digit
  _ <- endOfLine
  times <- numOrX `sepBy` char ','
  return (start, times)
  where numOrX = (try (char 'x') *> pure Nothing) <|> (Just <$> read <$> many1 digit)

solveSystem :: [(Integer, Integer)] -> Integer
solveSystem constraints = sum [a * b * (b `inverseMod` n)
                              | (a, n) <- constraints, let b = p `div` n]
                          `mod` p
  where p = product [n | (_, n) <- constraints]

main :: IO ()
main = do
  input <- getContents
  case parse stopsList "" input of
    Left err -> print err
    Right (start, times) ->
      let constraints = [(-a, bid) | (Just bid, a) <- zip times [0..]]
      in print $ solveSystem constraints
