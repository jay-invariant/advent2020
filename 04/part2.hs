#!/usr/bin/env stack
-- stack --resolver lts-16.24 script 

import Control.Monad
import Data.Either
import Text.Parsec

type Passport = [(String, String)]

passportParse :: Parsec String u Passport
passportParse = many1 keyval
  where keyval = do
          key <- count 3 lower
          _ <- char ':'
          val <- many1 (alphaNum <|> char '#')
          _ <- space
          return (key, val)

isValid :: Passport -> Bool
isValid passport = all satisfiesReq fieldReqs
  where satisfiesReq (key, parser) =
          maybe False (isRight . parse (parser >> eof) "") $ lookup key passport

fieldReqs :: [(String, Parsec String u ())]
fieldReqs =
  [ ("byr", do
        byr <- read <$> count 4 digit
        guard $ (byr :: Int) >= 1920 && byr <= 2002
    )
  , ("iyr", do
        iyr <- read <$> count 4 digit
        guard $ (iyr :: Int) >= 2010 && iyr <= 2020
    )
  , ("eyr", do
        eyr <- read <$> count 4 digit
        guard $ (eyr :: Int) >= 2020 && eyr <= 2030
    )
  , ("hgt", do
        num <- read <$> many1 digit
        try (string "cm" >> (guard $ (num :: Int) >= 150 && num <= 193))
          <|> (string "in" >> (guard $ num >= 59 && num <= 76))
    )
  , ("hcl", void $ char '#' >> count 6 hexDigit)
  , ("ecl", void $ choice $ fmap (try . string)
      ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"])
  , ("pid", void $ count 9 digit)
  ]

main :: IO ()
main = do
  input <- getContents
  case (parse (passportParse `sepBy` endOfLine) "" input) of
    Left err -> print err
    Right passports -> print $ length $ filter isValid $ passports
