#!/usr/bin/env stack
-- stack --resolver lts-16.24 script

import Data.List

countArrangements :: [Int] -> Int
countArrangements diffs = last table
  where table = [arrs n | n <- [0..length diffs]]
        arrs 0 = 1
        arrs n = let merge0 = table !! (n - 1)
                     merge1 = if n >= 2 &&
                                 diffs !! (n - 1) + diffs !! (n - 2) <= 3
                              then table !! (n - 2) else 0
                     merge2 = if n >= 3 &&
                                 diffs !! (n - 1) + diffs !! (n - 2)
                                 + diffs !! (n - 3) <= 3
                              then table !! (n - 3) else 0
                 in merge0 + merge1 + merge2

main :: IO ()
main = do
  input <- getContents
  let jolts = 0 : (sort $ fmap read $ lines input)
      diffs = zipWith (-) (tail jolts) jolts ++ [3]
  print $ countArrangements diffs

