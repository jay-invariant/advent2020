#!/usr/bin/env stack
-- stack --resolver lts-16.24 script

import Text.Parsec
import Data.Ix

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
        ticket = natural `sepBy` char ','
        natural = read <$> many1 digit

isValidField :: [Field] -> Int -> Bool
isValidField fields x = any inOneRange fields
  where inOneRange (Field _ r1 r2) = inRange r1 x || inRange r2 x

sumInvalid :: [Field] -> [Ticket] -> Int
sumInvalid fields tickets = sum $ filter (not . isValidField fields) $ concat tickets

main :: IO ()
main = do
  input <- getContents
  case parse parseInput "" input of
    Left err -> print err
    Right (Input fields _ nearTickets) -> print $ sumInvalid fields nearTickets
