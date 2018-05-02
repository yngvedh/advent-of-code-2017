module AoC.KnotHash.Parse (parseKnotInput, codes) where

import AoC.ParsePrimitives

import Text.Parsec
import Text.Parsec.String

import Data.Char (ord)

parseKnotInput :: String -> Either String ([Int], [Int])
parseKnotInput input = case parse knotInput "" input of
  Left error -> Left $ show error
  Right result -> Right (result, codes $ dropNewline input)

knotInput :: Parser [Int]
knotInput = do
  knot <- integer `sepBy` char ','
  char '\n'
  return knot

codes t = (map ord t)

dropNewline t = take ((length t) - 1) t