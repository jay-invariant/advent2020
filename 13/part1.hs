#!/usr/bin/env stack
-- stack --resolver lts-16.24 script

import Text.Parsec
import Data.Maybe
import Data.List

stopsList :: Parsec String u (Int, [Maybe Int])
stopsList = do
  start <- read <$> many1 digit
  _ <- endOfLine
  times <- numOrX `sepBy` char ','
  return (start, times)
  where numOrX = (try (char 'x') *> pure Nothing) <|> (Just <$> read <$> many1 digit)

firstBus :: Int -> [Int] -> (Int, Int)
firstBus start times = head $ sort [(nextBus bid, bid) | bid <- times]
  where nextBus bid = ((start + bid - 1) `div` bid) * bid

main :: IO ()
main = do
  input <- getContents
  case parse stopsList "" input of
    Left err -> print err
    Right (start, times) ->
      let (time, bid) = firstBus start $ catMaybes times
      in print (bid * (time - start))
          
          
