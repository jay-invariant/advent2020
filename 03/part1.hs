#!/usr/bin/env stack
-- stack --resolver lts-16.24 script 

lineHasTree :: String -> Int -> Bool
lineHasTree line index = line !! (index `mod` length line) == '#'

main :: IO ()
main = do
  treemap <- lines <$> getContents
  print $ length $ filter (uncurry lineHasTree) $ zip treemap [0,3..]
  
