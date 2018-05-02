module AoC.ParsePrimitives (integer) where

import Text.Parsec
import Text.Parsec.String

integer :: Parser Int
integer = do
  sign <- optionMaybe $ char '-'
  s <- many1 digit
  let val = (read s :: Int)
  case sign of
    Just _ -> return (-val)
    Nothing -> return val
