module AoC.Bridge.Parse (parseMagComps) where

import AoC.Misc (mapLeft)
import AoC.ParsePrimitives

import AoC.Bridge.Core

import Text.Parsec
import Text.Parsec.String

parseMagComps = mapLeft show . parse magComps ""

magComps :: Parser [MagComp]
magComps = magComp `sepEndBy` char '\n'

magComp :: Parser MagComp
magComp = do
  a <- integer
  char '/'
  b <- integer
  return $ makeMagComp a b