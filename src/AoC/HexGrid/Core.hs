module AoC.HexGrid.Core (
  HexDir (..), HexPath (..), HexPos (..),
  endPos, hexManhattanDist,
  pathPositions, farthestDist) where

data HexDir = N | NW | NE | S | SE | SW
  deriving (Eq, Show)

data HexPath = HexPath [HexDir]
  deriving (Eq, Show)

data HexPos = HexPos Int Int
  deriving (Eq, Show)

endPos :: HexPath -> HexPos
endPos (HexPath ds) = foldl stepDir (HexPos 0 0) ds

stepDir :: HexPos -> HexDir -> HexPos
stepDir (HexPos n se) N = HexPos (n+1) se
stepDir (HexPos n se) S = HexPos (n-1) se
stepDir (HexPos n se) SE = HexPos n (se+1)
stepDir (HexPos n se) NW = HexPos n (se-1)
stepDir (HexPos n se) SW = HexPos (n-1) (se-1)
stepDir (HexPos n se) NE = HexPos (n+1) (se+1)

hexManhattanDist :: HexPos -> Int
hexManhattanDist p@(HexPos n se) =
  (abs $ n - ne) + (abs $ se - ne) + (abs ne) where
  ne = northEastComponent p 

northEastComponent (HexPos n se) =
  if n * se < 0 then 0
  else if n < 0 then max n se
  else min n se

pathPositions :: HexPath -> [HexPos]
pathPositions (HexPath path) = reverse $ foldl step [] path where
  step :: [HexPos] -> HexDir -> [HexPos]
  step [] dir = [stepDir (HexPos 0 0) dir]
  step ps@(p:_) dir = p':ps where
    p' = stepDir p dir

farthestDist = maximum . map hexManhattanDist . pathPositions