module AoC.PermProm (parseDanceMoves, dance, makeDancer, dancerName) where

import qualified AoC.PermProm.Core as C
import qualified AoC.PermProm.Parse as P

parseDanceMoves = P.parseDanceMoves
dance = C.dance

makeDancer = C.Dancer
dancerName (C.Dancer c) = c