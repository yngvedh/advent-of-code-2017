module AoC.PassPhrase (isValid, isValidNoAnagrams) where

import Data.List (nub, sort)

isValid :: String -> Bool
isValid passphrase = ws == (nub ws) where
  ws = words passphrase

isValidNoAnagrams :: String -> Bool
isValidNoAnagrams passphrase = ws == (nub ws) where
  ws = map sort $ words passphrase