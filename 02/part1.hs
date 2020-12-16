#!/usr/bin/env stack
-- stack --resolver lts-16.24 script 

import Text.Parsec

data Entry = Entry Int Int Char String

entryParse :: Parsec String () Entry
entryParse = do
  lowerBound <- read <$> many digit
  _ <- char '-'
  upperBound <- read <$> many digit
  _ <- many space
  limitedChar <- lower
  _ <- char ':'
  _ <- many space
  password <- many lower
  _ <- endOfLine
  return $ Entry lowerBound upperBound limitedChar password
  
validateEntry :: Entry -> Bool
validateEntry (Entry lb ub c pass) =
  let l = length $ filter (==c) pass in
    l >= lb && l <= ub
  
main :: IO ()
main = do
  input <- getContents
  case (parse (many entryParse) "" input) of
    Left err -> print err
    Right entries -> print $ length $ filter validateEntry entries
