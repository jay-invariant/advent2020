#!/usr/bin/env stack
-- stack --resolver lts-16.24 script

{-# LANGUAGE FlexibleContexts #-}


import qualified Data.IntMap as M
import Text.Parsec

data Rule = Atom Char
          | Compound [[Int]]
  deriving Show

type Rules = M.IntMap Rule

rules :: Parsec String u Rules
rules = M.fromList <$> rule `sepEndBy` endOfLine
  where rule = do
          i <- nat
          _ <- string ": "
          r <- atom <|> compound
          return (i, r)
        atom = Atom <$> (char '"' *> letter <* char '"')
        compound = Compound <$> many nat `sepBy` string "| "
        nat = read <$> many1 digit <* many (char ' ')

rulesAndWords :: Parsec String u (Rules, [String])
rulesAndWords = do
  rs <- rules
  _ <- endOfLine
  ws <- many1 letter `sepEndBy` endOfLine
  return (rs, ws)

rulesToParse :: Rules -> Parsec String u String
rulesToParse rs = nthRule 0 <* eof
  where nthRule n = case rs M.! n of
          Atom c -> (:[]) <$> char c
          Compound terms -> choice [try $ mconcat (nthRule <$> t) | t <- terms]

countParses :: Parsec String () a -> [String] -> Int
countParses p ss = length $ filter doesParse ss
  where doesParse s = case parse p "" s of
          Left _ -> False
          Right _ -> True
  
main :: IO ()
main = do
  input <- getContents
  case parse rulesAndWords "" input of
    Left err -> print $ err
    Right (rs, ws) -> print $ countParses (rulesToParse rs) ws
