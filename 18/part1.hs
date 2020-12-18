#!/usr/bin/env stack
-- stack --resolver lts-16.24 script

import Text.Parsec
import Data.Foldable

data Op = Plus | Times
  deriving Show

data Expr = Compound Expr [(Op, Expr)]
          | Atom Int
  deriving Show

expr :: Parsec String u Expr
expr = Compound <$> subexpr <*> many ((,) <$> op <*> subexpr)
  where subexpr = inParens <|> value
        inParens = char '(' *> expr <* char ')'
        value = Atom <$> read <$> many1 digit
        op = char ' ' *> (plus <|> times) <* char ' '
        plus = char '+' *> pure Plus
        times = char '*' *> pure Times

eval :: Expr -> Int
eval (Atom x) = x
eval (Compound e ops) = foldl' doOp (eval e) ops
  where doOp acc (Plus, x) = acc + eval x
        doOp acc (Times, x) = acc * eval x

main :: IO ()
main = do
  input <- getContents
  case parse (expr `sepEndBy` endOfLine) "" input of
    Left err -> print err
    Right exprs -> print $ sum $ eval <$> exprs
  
