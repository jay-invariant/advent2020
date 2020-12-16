#!/usr/bin/env stack
-- stack --resolver lts-16.24 script

import Data.Array
import Text.Parsec
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (haskellDef)

lexer = P.makeTokenParser haskellDef
integer = P.integer lexer

data Instruction = Acc Int | Jmp Int | Nop
  deriving Show

type Program = Array Int Instruction

parseInstruction :: Parsec String u Instruction
parseInstruction = acc <|> jmp <|> nop
  where acc = try (string "acc ") *> (Acc <$> fromInteger <$> integer)
        jmp = try (string "jmp ") *> (Jmp <$> fromInteger <$> integer)
        nop = try (string "nop ") *> integer *> pure Nop

parseProgram :: Parsec String u Program
parseProgram = do
  instructions <- many parseInstruction
  return $ listArray (0, length instructions - 1) instructions

accumBeforeSecondRun :: Program -> Int
accumBeforeSecondRun program = go 0 0 (listArray (bounds program) (repeat False))
  where go pc acc visited | visited ! pc = acc
                          | otherwise = let visited' = visited // [(pc, True)] in
                              case program ! pc of
                                Acc arg -> go (pc + 1) (acc + arg) visited'
                                Jmp arg -> go (pc + arg) acc visited'
                                Nop -> go (pc + 1) acc visited'

main :: IO ()
main = do
  input <- getContents
  case parse parseProgram "" input of
    Left err -> print err
    Right prog -> print $ accumBeforeSecondRun prog
  
