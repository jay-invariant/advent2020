#!/usr/bin/env stack
-- stack --resolver lts-16.24 script

import Data.Array

data Square = Occupied | Empty | Floor
  deriving Eq

type Seating = Array (Int, Int) Square

applyRule :: Seating -> Seating
applyRule seating = array (bounds seating) [(loc, seat loc) | loc <- range (bounds seating)]
  where seat loc = case seating ! loc of
          Empty | numSeen loc == 0 -> Occupied
          Occupied | numSeen loc >= 5 -> Empty
          x -> x
        numSeen loc = length $ filter id $ fmap (sees loc) dirs
        dirs = [(x, y) | x <- [-1..1], y <- [-1..1], (x,y) /= (0,0)]
        sees loc dir = let loc' = (fst loc + fst dir, snd loc + snd dir) in
                                 if inRange (bounds seating) loc'
                                 then case seating ! loc' of
                                     Occupied -> True
                                     Empty -> False
                                     Floor -> sees loc' dir
                                 else False
                                 

fixed :: Eq a => (a -> a) -> a -> a
fixed f x = let y = f x in
              if y == x then x
              else fixed f y

toSquare :: Char -> Square
toSquare 'L' = Empty
toSquare '#' = Occupied
toSquare '.' = Floor
toSquare _ = error "bad square"

countOcc :: Square -> Int
countOcc Occupied = 1
countOcc _ = 0

main :: IO ()
main = do
  input <- getContents
  let ll = lines input
      rowLen = length $ head ll
      numRows = length ll
      seating = listArray ((1,1), (numRows, rowLen)) $ fmap toSquare $ concat ll
      stable = fixed applyRule seating
  print $ sum $ fmap countOcc stable
