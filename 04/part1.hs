#!/usr/bin/env stack
-- stack --resolver lts-16.24 script 

import Text.Parsec

type Passport = [(String, String)]

passportParse :: Parsec String () Passport
passportParse = many1 keyval
  where keyval = do
          key <- count 3 lower
          _ <- char ':'
          val <- many1 (alphaNum <|> char '#')
          _ <- space
          return (key, val)

isValid :: Passport -> Bool
isValid passport = all containsKey requiredKeys
  where containsKey key = key `elem` fmap fst passport

requiredKeys :: [String]
requiredKeys = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

main :: IO ()
main = do
  input <- getContents
  case (parse (passportParse `sepBy` endOfLine) "" input) of
    Left err -> print err
    Right passports -> print $ length $ filter isValid $ passports
