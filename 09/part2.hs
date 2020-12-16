#!/usr/bin/env stack
-- stack --resolver lts-16.24 script

import Data.List

firstNonSum :: Int -> [Int] -> Int
firstNonSum prelen xs = head nonSums
  where preambles = fmap (take prelen) $ tails xs
        nonSums = fmap fst $ filter (not . hasSum) $ zip (drop prelen xs) preambles
        hasSum (s, pre) = any (==s) [x + y | x <- pre, y <- pre]

main :: IO ()
main = do
  input <- getContents
  let xs = fmap read $ lines input
      ns = firstNonSum 25 xs
      ranges = inits =<< tails xs
      goodRange = head $ filter ((==ns) . sum) ranges
  print (minimum goodRange + maximum goodRange)
