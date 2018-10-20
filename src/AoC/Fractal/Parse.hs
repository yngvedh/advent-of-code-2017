module AoC.Fractal.Parse (parseRules) where

import Text.Parsec
import Text.Parsec.String

import AoC.Fractal.Core
import AoC.Misc (mapLeft)

parseRules :: String -> Either String RuleSet
parseRules = mapLeft show . parse rules ""

rules :: Parser RuleSet
rules = do
  rs <- rule `sepEndBy` char '\n'
  return $ RuleSet rs

rule :: Parser Rule
rule = do
  from <- image
  string " => "
  to <- image
  return $ Rule from to

image :: Parser Image
image = do
  ps <- many1 pixel `sepBy` char '/'
  return $ Image ps

pixel :: Parser Pixel
pixel = Off <$ char '.' <|>
        On  <$ char '#'
