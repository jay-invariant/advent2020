#!/usr/bin/env stack
-- stack --resolver lts-16.24 script

import Data.IntMap

spokenNumbers :: [Int] -> [Int]
spokenNumbers starting = starting ++ remainder
  where remainder = go (length starting + 1) (last starting)
          $ fromList $ zip starting [1 .. length starting - 1]
        go thisTurn lastNum spoken = 
          thisNum : go (thisTurn + 1) thisNum (insert lastNum (thisTurn - 1) spoken)
          where thisNum = case spoken !? lastNum of
                            Nothing -> 0
                            Just oldTurn -> thisTurn - 1 - oldTurn

main :: IO ()
main = print $ spokenNumbers [2,15,0,9,1,20] !! 2019
