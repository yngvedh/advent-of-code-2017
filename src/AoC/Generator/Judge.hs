module AoC.Generator.Judge (
  isMatching, isValuesMatching,
  judgeGenerators) where

import AoC.Generator.Core

bits :: Int -> Int
bits a = a `mod` 65536

isMatching :: Generator -> Generator -> Bool
isMatching a b = bits (value a) == bits (value b)

isValuesMatching a b = bits a == bits b

judgeGenerators :: Int -> Generator -> Generator -> Int
judgeGenerators n g1 g2 = length . filter ((==)True) . take n $ zipWith isValuesMatching (generateAllValues g1) (generateAllValues g2)

