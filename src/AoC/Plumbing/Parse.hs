module AoC.Plumbing.Parse (parsePlumbing) where

import Text.Parsec
import Text.Parsec.String

import AoC.Misc
import AoC.Plumbing.Core
import AoC.ParsePrimitives

import Data.List (nub)

parsePlumbing :: String -> Either String Village
parsePlumbing = mapLeft show . parse plumbing ""

plumbing :: Parser Village
plumbing = do
  pipess <- pipes `sepEndBy` char '\n'
  return . Village . nub . concat $ pipess

makeAssoc from to = [(from, to), (to, from)]

pipes :: Parser [(Program, Program)]
pipes = do 
  from <- program
  string " <-> "
  tos <- program `sepBy` string ", "
  return $ concat . map (makeAssoc from) $ tos

program :: Parser Program
program = do
  id <- integer
  return $ Program id