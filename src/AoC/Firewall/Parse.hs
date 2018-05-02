module AoC.Firewall.Parse (parseFirewall) where

import AoC.Firewall.Core
import AoC.ParsePrimitives
import AoC.Misc (mapLeft)

import Text.Parsec
import Text.Parsec.String

parseFirewall = mapLeft show . parse firewall ""

firewall :: Parser Firewall
firewall = do
  layers <- layer `sepEndBy` char '\n'
  return $ Firewall layers

layer :: Parser Layer
layer = do
  pos <- integer
  string ": "
  depth <- integer
  return $ Layer pos depth

