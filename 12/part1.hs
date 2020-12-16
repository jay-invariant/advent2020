#!/usr/bin/env stack
-- stack --resolver lts-16.24 script

import Prelude hiding (Right, Left)

data Instruction = Instruction Action Int
  deriving Show

data Action = North
            | South
            | East
            | West
            | Left
            | Right
            | Forward
  deriving Show
data State = State (Int, Int) (Int, Int)

readInstr :: String -> Instruction
readInstr (c : s) = Instruction action value
  where action = case c of
          'N' -> North
          'S' -> South
          'E' -> East
          'W' -> West
          'F' -> Forward
          'L' -> Left
          'R' -> Right
          _ -> error "bad instruction"
        value = read s
readInstr [] = error "empty instruciton"

addPair :: (Int, Int) -> (Int, Int) -> (Int, Int)
addPair p1 p2 = (fst p1 + fst p2, snd p1 + snd p2)

scalePair :: Int -> (Int, Int) -> (Int, Int)
scalePair s (x, y) = (s * x, s * y)

followDir :: Instruction -> State -> State
followDir (Instruction action value) (State pos dir) =
  case action of
    North -> State (addPair pos (0, value)) dir
    South -> State (addPair pos (0, -value)) dir
    East  -> State (addPair pos (value, 0)) dir
    West  -> State (addPair pos (-value, 0)) dir
    Left  -> State pos $ iter (value `div` 90) rotateLeft dir
    Right -> State pos $ iter (value `div` 90) rotateRight dir
    Forward -> State (addPair pos $ scalePair value dir) dir
  where rotateLeft (a, b) = (-b, a)
        rotateRight (a, b) = (b, -a)

iter :: Int -> (a -> a) -> a -> a
iter n f | n < 0 = error "negative iter"
         | otherwise = foldr (.) id $ replicate n f

main :: IO ()
main = do
  input <- getContents
  let instrs = fmap readInstr $ lines input
      State (x, y) _ = foldr (flip (.)) id (fmap followDir instrs) $ State (0, 0) (1, 0)
  print (abs x + abs y)
