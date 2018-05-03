module AoC.PermProm.Core (
  DanceMove(..), Dancer(..),
  spin, exchange, partner, move,
  dance) where

import Data.List (elemIndex)
import Data.Maybe (fromJust)

data DanceMove = Spin Int | Exchange Int Int | Partner Dancer Dancer
  deriving (Show, Eq)

data Dancer = Dancer Char
  deriving (Show, Eq)

spin :: Int -> [Dancer] -> [Dancer]
spin n ds = take m . drop n' . cycle $ ds where
  m = length ds
  n' = m - n

exchange :: Int -> Int -> [Dancer] -> [Dancer]
exchange i j ds = if i == j
  then ds
  else pre ++ (b:mid) ++ (a:post) where
    pre = take i' ds
    a = head . drop i' $ ds
    mid = take (j' - i' - 1) . drop (i' + 1) $ ds
    b = head . drop j' $ ds
    post = drop (j' + 1) ds
    i' = min i j
    j' = max i j

partner :: Dancer -> Dancer -> [Dancer] -> [Dancer]
partner a b ds = exchange (fromJust $ elemIndex a ds) (fromJust $ elemIndex b ds) ds

move :: DanceMove -> [Dancer] -> [Dancer]
move (Spin n) = spin n
move (Exchange i j) = exchange i j
move (Partner a b) = partner a b

dance :: [DanceMove] -> [Dancer] -> [Dancer]
dance = flip $ foldl (flip move)