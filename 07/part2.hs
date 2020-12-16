#!/usr/bin/env stack
-- stack --resolver lts-16.24 script

import Text.Parsec
import Data.Array
import qualified Data.Map as M

parseRules :: Parsec String u (M.Map String [(Int, String)])
parseRules = M.fromList <$> (parseLine `sepEndBy` endOfLine)
  where parseLine = do
          container <- bagType
          _ <- string " bags contain "
          contained <- (try $ string "no other bags" *> pure [])
            <|> (bagWithQty `sepBy` string ", ")
          _ <- char '.'
          return (container, contained)
          
        bagWithQty = do
          num <- read <$> many1 digit
          _ <- space
          bag <- bagType
          _ <- string " bag"
          optional $ char 's'
          return (num, bag)
          
        bagType = do
          first <- many lower
          _ <- space
          last <- many lower
          return $ first ++ " " ++ last

bagSize :: String -> M.Map String [(Int, String)] -> Int
bagSize parent rules =
  let children = rules M.! parent
      numChildren = sum $ fmap (\(num, child) -> num * bagSize child rules) children
  in numChildren + 1

main :: IO ()
main = do
  input <- getContents
  case parse parseRules "" input of
    Left err -> print err
    Right rules -> print $ bagSize "shiny gold" rules - 1
