module AoC.Duet.Parse (parseInstructions) where

import Text.Parsec
import Text.Parsec.String

import Prelude hiding (snd, mod)

import AoC.Duet.Core
import AoC.Misc (mapLeft)
import AoC.ParsePrimitives

parseInstructions :: String -> Either String [Instruction]
parseInstructions = mapLeft show . parse instructions ""

instructions :: Parser [Instruction]
instructions = instruction `sepEndBy` char '\n'

instruction :: Parser Instruction
instruction = try snd <|> try set <|> try add <|> try mul <|> try mod <|> try rcv <|> try jgz

register :: Parser Register
register = do
  reg <- many1 letter
  return $ Register reg

snd :: Parser Instruction
snd = do
  string "snd "
  hz <- value
  return $ Snd hz

set = instrRV "set" Set
add = instrRV "add" Add
mul = instrRV "mul" Mul
mod = instrRV "mod" Mod

rcv :: Parser Instruction
rcv = do
  string "rcv "
  r <- register
  return $ Rcv r

jgz = instrRV "jgz" Jgz

instrRV :: String -> (Register -> Value -> Instruction) -> Parser Instruction
instrRV name ctor = do
  string name
  char ' '
  reg <- register
  char ' '
  val <- value
  return $ ctor reg val

value :: Parser Value
value = do
  lit <- optionMaybe integer
  case lit of
    Just v -> return $ LiteralValue v
    Nothing -> do
      reg <- register
      return $ RegisterValue reg


