module AoC.Sporifica.Core (
  Nodes (..), Node(..),
  makeGrid) where

import qualified Data.Map.Strict as S

data Nodes = Nodes (S.Map (Int,Int) Node)
  deriving (Show, Eq)

data Node = Clean | Infected
  deriving (Show, Eq)

makeGrid :: [[Node]] -> Nodes
makeGrid cs = Nodes . S.fromList $ assocList where
  assocList :: [((Int,Int),Node)]
  assocList = concat . zipWith f [(-h')..h'] . map (zip [(-w')..w']) $ cs
  w = length . head $ cs
  h = length cs
  w' = (w-1) `div` 2
  h' = (h-1) `div` 2
  f :: Int -> [(Int, Node)] -> [((Int,Int),Node)]
  f y = map (\(x, c) -> ((x, y), c))