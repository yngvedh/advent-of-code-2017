module AoC.Generator.Parse (parseGenerators) where

import AoC.Generator.Core

parseGenerators :: String -> Either String ((Int,Int,Int), (Int,Int,Int))
parseGenerators s = Right ((s1, 4, 16807), (s2, 8, 48271)) where
  [s1,s2] = map (read :: String -> Int) . map (drop 24) . lines $ s