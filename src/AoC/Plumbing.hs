module AoC.Plumbing (
  parsePlumbing,
  getGroup, getGroups) where

import qualified AoC.Plumbing.Parse as P
import qualified AoC.Plumbing.Core as C

parsePlumbing = P.parsePlumbing
getGroup v id = C.getGroup v (C.Program id)
getGroups = C.getGroups