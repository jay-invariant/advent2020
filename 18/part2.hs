#!/usr/bin/env stack
-- stack --resolver lts-16.24 script

import Text.Parsec

data Op = Plus | Times
  deriving Show

data Expr = ProductOfFactors [[Expr]]
          | Value Int
  deriving Show

expr :: Parsec String u Expr
expr = ProductOfFactors <$> factor `sepBy1` (try $ string " * ")
  where factor = subexpr `sepBy1` (try $ string " + ")
        subexpr = inParens <|> value
        inParens = char '(' *> expr <* char ')'
        value = Value <$> read <$> many1 digit

eval :: Expr -> Int
eval (Value x) = x
eval (ProductOfFactors factors) = product $ (sum . fmap eval) <$> factors

main :: IO ()
main = do
  input <- getContents
  case parse (expr `sepEndBy` endOfLine) "" input of
    Left err -> print err
    Right exprs -> print $ sum $ eval <$> exprs
  
