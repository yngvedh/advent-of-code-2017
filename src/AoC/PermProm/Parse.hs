module AoC.PermProm.Parse (parseDanceMoves) where

import Text.Parsec
import Text.Parsec.String

import AoC.PermProm.Core (DanceMove(..), Dancer(..))
import AoC.ParsePrimitives
import AoC.Misc (mapLeft)

parseDanceMoves = mapLeft show . parse danceMoves ""

danceMoves :: Parser [DanceMove]
danceMoves = do
  moves <- danceMove `sepBy` char ','
  char '\n'
  return moves

danceMove :: Parser DanceMove
danceMove = spin <|> exchange <|> partner

spin :: Parser DanceMove
spin = do
  char 's'
  n <- integer
  return $ Spin n


exchange :: Parser DanceMove
exchange = do
  char 'x'
  a <- integer
  char '/'
  b <- integer
  return $ Exchange a b

partner :: Parser DanceMove
partner = do
  char 'p'
  a <- anyChar
  char '/'
  b <- anyChar
  return $ Partner (Dancer a) (Dancer b)

