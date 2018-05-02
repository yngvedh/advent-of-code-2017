module AoC.CircularMemory (
  distanceToAddress,
  initialValueOfAddress, initialValues,
  locations,
  locationOfAddress, addressOfLocation,
  segmentOfLocation, Segment(..)) where

import Data.List (find)
import Data.Maybe (fromJust)

data Segment = LeftSegment | TopSegment | RightSegment | BottomSegment
  deriving (Show, Eq)

data Ring = Ring Int
  deriving (Show, Eq)

distanceToAddress :: Int -> Int
distanceToAddress a = (abs x) + (abs y) where
  (x,y) = locationOfAddress a

initialValueOfAddress :: Int -> Int
initialValueOfAddress = initialValueOfLocation . locationOfAddress

initialValueOfLocation :: (Int, Int) -> Int
initialValueOfLocation (0, 0) = 1
initialValueOfLocation loc@(x, y) = sum $ map initialValueOfLocation initializedNeightbours where
  initializedNeightbours = filter (\n -> adr > (addressOfLocation n)) allNeighbours
  allNeighbours = [(x+x',y+y')|x' <- [(-1)..1], y' <- [(-1)..1], x' /= 0 || y' /= 0]
  adr = addressOfLocation loc

ringOfAddress :: Int -> Ring
ringOfAddress 1 = Ring 0
ringOfAddress a = Ring $ fromJust $ find (\x -> (x*2+1)^2 >= a) [1..]

locationOfAddress :: Int -> (Int, Int)
locationOfAddress 1 = (0,0)
locationOfAddress a = let
  ring = ringOfAddress a
  first = firstAddressOfRing ring
  last = lastAddressOfRing ring
  ringOffset = offsetOfAdressInRing a ring
  segmentLength = segmentSizeOfRing ring
  segment = ringOffset `div` segmentLength
  segmentOffset = ringOffset `mod` segmentLength
  posBase = coordOfRing ring
  in case segment of
      0 -> (posBase, -posBase + 1 + segmentOffset)
      1 -> (posBase - 1 - segmentOffset, posBase)
      2 -> (-posBase, posBase - 1 - segmentOffset)
      3 -> (-posBase + 1 + segmentOffset, -posBase)

offsetOfAdressInRing a r = a - (firstAddressOfRing r)

ringOfLocation :: (Int, Int) -> Ring
ringOfLocation (x, y) = Ring $ max (abs x) (abs y)

firstAddressOfRing (Ring n) = (lastAddressOfRing $ Ring $ n-1) + 1
lastAddressOfRing (Ring n) = (n*2+1)^2
segmentSizeOfRing (Ring n) = n*2
coordOfRing (Ring n) = n

segmentOfLocation :: (Int, Int) -> Segment
segmentOfLocation loc@(x, y) = let
  coord = coordOfRing $ ringOfLocation loc in
    if x == coord && y /= (-coord) then RightSegment
    else if y == coord && x /= coord then TopSegment
    else if x == (-coord) && y /= coord then LeftSegment
    else if y == (-coord) && x /= (-coord) then BottomSegment
    else undefined

addressOfLocation :: (Int, Int) -> Int
addressOfLocation (0,0) = 1
addressOfLocation loc@(x,y) = let
  ring = ringOfLocation loc
  first = firstAddressOfRing ring
  last = lastAddressOfRing ring
  segmentLength = segmentSizeOfRing ring
  coord = coordOfRing ring in
    case segmentOfLocation loc of
      RightSegment  -> (coord-1) + y + first
      TopSegment    -> (coord-1) - x + first + segmentLength
      LeftSegment   -> (coord-1) - y + first + segmentLength*2
      BottomSegment -> (coord-1) + x + first + segmentLength*3

initialValues = map initialValueOfAddress [1..]
locations = map locationOfAddress [1..]

--17 16 15 14 13
--18  5  4  3 12 
--19  6  1  2 11 28
--20  7  8  9 10 27
--21 22 23 24 25 26
--               49
--   3   3210
--   2     |    0
-- y-1     x    1-y    x 
--   0          2      |
--              3     0123
