module AoC.Tubes.Parse (parseTubes) where

import Data.Char (isLetter)

import AoC.Focus.List
import AoC.Tubes.Core

parseTubes :: String -> Either String Tubes
parseTubes input = do
  tubeLists <- mapM (mapM toTube) . lines $ input
  squareLists <- verifySquare . filter (not . null) $ tubeLists
  return $ makeTubes squareLists

verifySquare :: [[Tube]] -> Either String [[Tube]]
verifySquare ls = if square then Right ls else Left "Input is not square." where
  square = all ((==) len) . map length $ ls
  len = length . head $ ls

toTube :: Char -> Either String Tube
toTube '|' = Right Vertical
toTube '-' = Right Horizontal
toTube '+' = Right Cross
toTube ' ' = Right Space
toTube c = if isLetter c then Right $ Letter c else Left $ "Unrecognized tube in input: " ++ [c]

