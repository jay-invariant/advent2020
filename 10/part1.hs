#!/usr/bin/env stack
-- stack --resolver lts-16.24 script

import Data.List

main :: IO ()
main = do
  input <- getContents
  let jolts = 0 : (sort $ fmap read $ lines input) :: [Int]
      diffs = zipWith (-) (tail jolts) jolts
      ones = length $ filter (==1) diffs
      threes = 1 + (length $ filter (==3) diffs)
  print ones
  print threes
  print (ones * threes)

