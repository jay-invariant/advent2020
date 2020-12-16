#!/usr/bin/env stack
-- stack --resolver lts-16.24 script

import Text.Parsec
import Data.Bits
import Data.IntMap
import Control.Monad.State

data Instruction = Mask Int Int
                 | Assign Int Int
  deriving Show

data ProgState = ProgState Int Int (IntMap Int)

parseProgram :: Parsec String u [Instruction]
parseProgram = parseInstruction `sepEndBy` endOfLine
  where parseInstruction = parseMask <|> parseAssign
        parseMask = do
          _ <- try $ string "mask = "
          mask <- count 36 $ oneOf "01X"
          let andMask = sum [shift 1 e | (b, e) <- zip mask [35,34..0], b /= '0']
              orMask = sum [shift 1 e | (b, e) <- zip mask [35,34..0], b == '1']
          return $ Mask andMask orMask
        parseAssign = do
          _ <- string "mem["
          addr <- read <$> many digit
          _ <- string "] = "
          value <- read <$> many digit
          return $ Assign addr value

runInstr :: Instruction -> ProgState -> ProgState
runInstr (Mask am om) (ProgState _ _ mem) = ProgState am om mem
runInstr (Assign addr value) (ProgState am om mem) =
  ProgState am om $ insert addr ((value .&. am) .|. om) mem

main :: IO ()
main = do
  input <- getContents
  case parse parseProgram "" input of
    Left err -> print err
    Right prog ->
      let start = ProgState 0 0 empty
          progM = sequence_ $ fmap (modify . runInstr) prog
          ProgState _ _ mem = execState progM start
      in print $ foldl' (+) 0 mem
