#!/usr/bin/env stack
-- stack --resolver lts-16.24 script

main :: IO ()
main = do
  nums <- fmap read <$> lines <$> getContents :: IO [Int]
  let solns = [a * b | a <- nums, b <- nums, a + b == 2020]
  putStrLn $ case solns of
    a : _ -> show a
    [] -> "No solutions"

