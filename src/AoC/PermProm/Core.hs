module AoC.PermProm.Core (
  DanceMove(..), Dancer(..),
  spin, exchange, partner, move,
  dance, repeatDance) where

import Data.List (elemIndex, nubBy)
import Data.Maybe (fromJust)
import Debug.Trace

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

repeatDance :: Int -> [DanceMove] -> [Dancer] -> [Dancer]
repeatDance n ms ds = c !! n' where
  n' = n `mod` l
  l = length c
  c = danceCycle ms ds

danceCycle :: [DanceMove] -> [Dancer] -> [[Dancer]]
danceCycle ms ds = ds:c where
  c = danceCycle' ds ms ds
  cut = length c - 1
  danceCycle' start ms ds = if ds' == start then [] else ds':(danceCycle' start ms ds') where
    ds' = dance ms ds
