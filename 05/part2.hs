#!/usr/bin/env stack
-- stack --resolver lts-16.24 script

import Text.Parsec
import Data.Foldable
import Data.List

data Seat = Seat Int Int
  deriving Show

parseBinary :: Char -> Char -> Int -> Parsec String u Int
parseBinary zero one len = do
  digits <- count len $ (try (char one) *> pure 1) <|> (char zero *> pure 0)
  return $ foldl' (\accum d -> accum * 2 + d) 0 digits
  
parseSeat :: Parsec String u Seat
parseSeat = do
  row <- parseBinary 'F' 'B' 7
  col <- parseBinary 'L' 'R' 3
  return $ Seat row col

seatId :: Seat -> Int
seatId (Seat row col) = row * 8 + col

missingSeats :: [Int] -> [(Int, Int)]
missingSeats seats =
  let sorted = sort seats in
    filter (\(x, y) -> y /= x + 1) $ zip sorted (tail sorted)
  
main :: IO ()
main = do
  input <- getContents
  case parse (parseSeat `sepEndBy` endOfLine) "" input of
    Left err -> print err
    Right seats -> print $ missingSeats $ fmap seatId seats
