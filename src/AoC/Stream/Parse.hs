module AoC.Stream.Parse (parseStream) where

import AoC.Stream.Core

import Text.Parsec
import Text.Parsec.String

import Data.Maybe (catMaybes)

parseStream :: String -> Either String Group
parseStream input = case parse group "" input of
  Left error -> Left . show $ error
  Right g -> Right g

group :: Parser Group
group = garbage' <|> group' where
  garbage' = do
    char '<'
    g <- many garbageChar
    char '>'
    return $ Garbage $ catMaybes g
  group' = do
    char '{'
    gs <- group `sepBy` char ','
    char '}'
    return $ Group gs

garbageChar :: Parser (Maybe Char)
garbageChar = garbage' <|> escaped' where
  garbage' = do
    c <- noneOf ">!"
    return $ Just c
  escaped' = do
    char '!'
    anyChar
    return Nothing