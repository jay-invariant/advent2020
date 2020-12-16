#!/usr/bin/env stack
-- stack --resolver lts-16.24 script 

lineHasTree :: String -> Int -> Bool
lineHasTree line index = line !! (index `mod` length line) == '#'

slopes :: [(Int, Int)]
slopes = [(1, 1),
          (3, 1),
          (5, 1),
          (7, 1),
          (1, 2)]

countTrees :: [String] -> (Int, Int) -> Int
countTrees treemap (right, down) =
  length $ filter (uncurry lineHasTree) $ zip (sublist down treemap) [0, right..]

sublist :: Int -> [a] -> [a]
sublist n xs = go 0 xs
  where go _ [] = []
        go 0 (y : ys) = y : go (n - 1) ys
        go k (_ : ys) = go (k - 1) ys

main :: IO ()
main = do
  treemap <- lines <$> getContents
  print $ product $ fmap (countTrees treemap) slopes
