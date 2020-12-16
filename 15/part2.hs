#!/usr/bin/env stack
-- stack --resolver lts-16.24 script
{-# LANGUAGE BangPatterns #-}

import Data.IntMap.Strict

spokenNumber :: [Int] -> Int -> Int
spokenNumber starting n = if n >= length starting
                          then go
                               (length starting + 1)
                               (last starting)
                               (fromList $ zip starting [1 .. length starting - 1])
                               (n - length starting)
                          else starting !! (n - 1)
  where go _ !lastNum _ 0 = lastNum
        go !thisTurn !lastNum !spoken !k = go
                                          (thisTurn + 1)
                                          thisNum
                                          (insert lastNum (thisTurn - 1) spoken)
                                          (k - 1)
          where thisNum = case spoken !? lastNum of
                            Nothing -> 0
                            Just oldTurn -> thisTurn - 1 - oldTurn

main :: IO ()
main = print $ spokenNumber [2,15,0,9,1,20] 30000000
