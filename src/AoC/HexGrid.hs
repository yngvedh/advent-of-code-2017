module AoC.HexGrid (parseHexPath, hexManhattanDist, hexEndPos, hexFarthestDist) where

import qualified AoC.HexGrid.Core as C
import qualified AoC.HexGrid.Parse as P

parseHexPath = P.parsePath
hexManhattanDist = C.hexManhattanDist
hexEndPos = C.endPos
hexFarthestDist = C.farthestDist