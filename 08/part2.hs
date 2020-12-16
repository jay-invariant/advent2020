#!/usr/bin/env stack
-- stack --resolver lts-16.24 script

import Data.Array
import Data.Maybe
import Text.Parsec
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (haskellDef)

lexer = P.makeTokenParser haskellDef
integer = P.integer lexer

data Instruction = Acc Int | Jmp Int | Nop Int
  deriving Show

type Program = Array Int Instruction

parseInstruction :: Parsec String u Instruction
parseInstruction = acc <|> jmp <|> nop
  where acc = try (string "acc ") *> (Acc <$> fromInteger <$> integer)
        jmp = try (string "jmp ") *> (Jmp <$> fromInteger <$> integer)
        nop = try (string "nop ") *> (Nop <$> fromInteger <$> integer)

parseProgram :: Parsec String u Program
parseProgram = do
  instructions <- many parseInstruction
  return $ listArray (0, length instructions - 1) instructions

-- Solves the halting problem
terminalValue :: Program -> Maybe Int
terminalValue program = go 0 0 (listArray (bounds program) (repeat False))
  where go pc acc visited
          | pc == 1 + (snd $ bounds program) = Just acc
          | visited ! pc = Nothing
          | otherwise = let visited' = visited // [(pc, True)] in
                          case program ! pc of
                            Acc arg -> go (pc + 1) (acc + arg) visited'
                            Jmp arg -> go (pc + arg) acc visited'
                            Nop _ -> go (pc + 1) acc visited'

fixedPrograms :: Program -> [Program]
fixedPrograms prog = do
  indexToFix <- range $ bounds prog
  case prog ! indexToFix of
    Jmp arg -> [prog // [(indexToFix, Nop arg)]]
    Nop arg -> [prog // [(indexToFix, Jmp arg)]]
    _ -> []

main :: IO ()
main = do
  input <- getContents
  case parse parseProgram "" input of
    Left err -> print err
    Right prog -> print $ catMaybes $ fmap terminalValue $ fixedPrograms prog
  
