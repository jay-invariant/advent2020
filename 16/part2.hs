#!/usr/bin/env stack
-- stack --resolver lts-16.24 script
{-# LANGUAGE BangPatterns #-}

import Text.Parsec
import Data.Ix
import Data.List
import qualified Data.IntSet as S

data Input = Input [Field] Ticket [Ticket]
  deriving Show

data Field = Field String (Int, Int) (Int, Int)
  deriving Show

type Ticket = [Int]

parseInput :: Parsec String u Input
parseInput = Input <$> fields <*> myTicket <*> nearTickets
  where fields = ((try field) `sepEndBy` endOfLine) <* endOfLine
        field = Field
                <$> (many (lower <|> space) <* string ": ")
                <*> range <*> (string " or " *> range)
        range = (,) <$> natural <*> (char '-' *> natural)
        myTicket = string "your ticket:\n" *> ticket <* count 2 endOfLine
        nearTickets = string "nearby tickets:\n" *> (ticket `sepEndBy` endOfLine)
        ticket = natural `sepBy1` char ','
        natural = read <$> many1 digit

isValidField :: [Field] -> Int -> Bool
isValidField fields x = any inOneRange fields
  where inOneRange (Field _ r1 r2) = inRange r1 x || inRange r2 x

isValidTicket :: [Field] -> Ticket -> Bool
isValidTicket fields = all (isValidField fields)

validPositions :: Field -> Ticket -> S.IntSet
validPositions field ticket = S.fromList [pos | (pos, val) <- zip [0..] ticket,
                                        isValidField [field] val]

inferPosition :: [Ticket] -> Field -> [Int]
inferPosition tickets field = S.toList $ foldr1 S.intersection $ fmap (validPositions field) tickets

inferPositions :: [Field] -> [Ticket] -> [(String, Int)]
inferPositions fields tickets = case solutions of
                                  soln : [] -> zip (fmap fst positions) (reverse soln)
                                  _ -> error "Not unique solution"
  where solutions = go positions [[]]
        go ((_, p) : ps) paths = go ps [i : path | path <- paths, i <- p, not $ i `elem` path]
        go [] accum = accum
        positions = sortOn (length . snd) $ fmap onField fields
        onField f@(Field name _ _) = (name, inferPosition tickets f)
         

main :: IO ()
main = do
  input <- getContents
  case parse parseInput "" input of
    Left err -> print err
    Right (Input fields myTicket nearTickets) ->
      let goodTickets = filter (isValidTicket fields) nearTickets
          positions = inferPositions fields goodTickets
          departures = filter ((== "departure") . take 9 . fst) positions
      in print $ product [myTicket !! i | (_, i) <- departures]
