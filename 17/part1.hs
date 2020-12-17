#!/usr/bin/env stack
-- stack --resolver lts-16.24 script

import Data.Array
import Data.Function.HT
import Data.Foldable

type Space = Array (Int, Int, Int) Bool

-- Coords are all (z, y, x), makes it easier

cycleSpace :: Space -> Space
cycleSpace old = listArray newBounds [isActive p | p <- range newBounds]
  where isActive p = case (isOldActive p, activeNeighbors p) of
                       (True, 2) -> True
                       (True, 3) -> True
                       (False, 3) -> True
                       _ -> False
        isOldActive p = inRange oldBounds p && old ! p
        activeNeighbors p@(x, y, z) =
          sum [1 | x' <- [x-1..x+1], y' <- [y-1..y+1], z' <- [z-1..z+1],
               let p' = (x', y', z'), p /= p', isOldActive p']
        oldBounds = bounds old
        newBounds = let ((z1, y1, x1), (z2, y2, x2)) = oldBounds
                    in ((z1-1, y1-1, x1-1), (z2+1, y2+1, x2+1))

initSpace :: String -> Space
initSpace str = listArray bnds $ concat lists
  where lists = fmap (fmap (=='#')) $ lines str
        rowLen = length $ head lists
        numRows = length lists
        bnds = ((0,0,0), (0, numRows - 1, rowLen - 1))

numActive :: Space -> Int
numActive = length . filter id . toList

main :: IO ()
main = do
  input <- getContents
  print $ numActive $ nest 6 cycleSpace $ initSpace input
