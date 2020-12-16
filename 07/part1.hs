#!/usr/bin/env stack
-- stack --resolver lts-16.24 script

import Text.Parsec
import Data.Array
import qualified Data.Map as M
import qualified Data.Graph as G

parseRules :: Parsec String u [(String, [String])]
parseRules = parseLine `sepEndBy` endOfLine
  where parseLine = do
          container <- bagType
          _ <- string " bags contain "
          contained <- (try $ string "no other bags" *> pure [])
            <|> (bagWithQty `sepBy` string ", ")
          _ <- char '.'
          return (container, contained)
          
        bagWithQty = do
          _ <- many1 digit
          _ <- space
          bag <- bagType
          _ <- string " bag"
          optional $ char 's'
          return bag
          
        bagType = do
          first <- many lower
          _ <- space
          last <- many lower
          return $ first ++ " " ++ last

allContainers :: String -> [(String, [String])] -> Int
allContainers start rules =
  let containers = fmap fst rules
      indices = M.fromList $ zip containers [0..]
      adjacencyList = fmap (fmap (indices M.!) . snd) rules
      containsG = listArray (0, length rules - 1) adjacencyList
      containedG = G.transposeG containsG
      eventualContainers = G.reachable containedG (indices M.! start)
  in length eventualContainers - 1

main :: IO ()
main = do
  input <- getContents
  case parse parseRules "" input of
    Left err -> print err
    Right rules -> print $ allContainers "shiny gold" rules
