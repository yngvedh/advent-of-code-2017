module AoC.HexGrid.Parse (parsePath) where

import Text.Parsec
import Text.Parsec.String

import AoC.HexGrid.Core

parsePath :: String -> Either String HexPath
parsePath input = case parse hexPath "" input of
  Right path -> Right path
  Left error -> Left . show $ error

hexPath :: Parser HexPath
hexPath = hexDir `sepBy` char ',' >>= (return . HexPath)

hexDir :: Parser HexDir
hexDir =
  NE <$ try (string "ne") <|>
  NW <$ try (string "nw") <|>
  N <$ string "n" <|>
  SE <$ try (string "se") <|>
  SW <$ try (string "sw") <|>
  S <$ string "s"