module AoC.PermProm (
  parseDanceMoves,
  repeatDance,
  dance,
  makeDancer, dancerName) where

import qualified AoC.PermProm.Core as C
import qualified AoC.PermProm.Parse as P

parseDanceMoves = P.parseDanceMoves
dance = C.dance

repeatDance = C.repeatDance

makeDancer = C.Dancer
dancerName (C.Dancer c) = c