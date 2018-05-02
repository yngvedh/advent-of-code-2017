module AoC.RegisterInstructions.Parse (parseInstructions) where

import AoC.RegisterInstructions.Core (Instruction(..), Comparison(..), Name(..))
import AoC.ParsePrimitives (integer)

import Text.Parsec
import Text.Parsec.String

parseInstructions :: String -> Either String [Instruction]
parseInstructions input = case parse instructions "" input of
  Right is -> Right is
  Left error -> Left $ show error

instructions :: Parser [Instruction]
instructions = instruction `sepEndBy` char '\n'

-- Format: b inc 5 if a > 1
instruction :: Parser Instruction
instruction = do
  n <- name
  char ' '
  dir <- 1 <$ string "inc" <|> (-1) <$ string "dec"
  char ' '
  offset <- integer
  string " if "
  cmpName <- name
  char ' '
  cmpOp <- comparisonOperator
  char ' '
  cmpVal <- integer
  return $ Instruction (Name n) (dir*offset) (Name cmpName) cmpOp cmpVal

name = many1 letter

comparisonOperator :: Parser Comparison
comparisonOperator =
  LessEq <$ try (string "<=")
  <|> GreaterEq <$ try (string ">=")
  <|> Less <$ string "<"
  <|> Greater <$ string ">"
  <|> Equal <$ string "=="
  <|> Inequal <$ string "!="
