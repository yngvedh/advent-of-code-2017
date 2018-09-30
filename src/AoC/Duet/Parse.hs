module AoC.Duet.Parse (parseInstructions) where

import Text.Parsec
import Text.Parsec.String

import Prelude hiding (snd, mod)

import AoC.Duet.Core hiding (instructions)
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
  Snd <$> value

set = instrRV "set" Set
add = instrRV "add" Add
mul = instrRV "mul" Mul
mod = instrRV "mod" Mod

rcv :: Parser Instruction
rcv = do
  string "rcv "
  Rcv <$> register

jgz = instrVV "jgz" Jgz

instrRV :: String -> (Register -> Value -> Instruction) -> Parser Instruction
instrRV name ctor = do
  string name
  char ' '
  reg <- register
  char ' '
  ctor reg <$> value

instrVV :: String -> (Value -> Value -> Instruction) -> Parser Instruction
instrVV name ctor = do
  string name
  char ' '
  val <- value
  char ' '
  ctor val <$> value
  
value :: Parser Value
value = do
  lit <- optionMaybe integer
  case lit of
    Just v -> return $ LiteralValue v
    Nothing -> RegisterValue <$> register


