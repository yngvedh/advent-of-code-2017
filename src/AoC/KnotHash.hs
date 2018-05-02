module AoC.KnotHash (
  parseKnotInput,
  computeKnotHash,
  computeFullKnotHash, showFullKnotHash) where

import qualified AoC.KnotHash.Core as C
import qualified AoC.KnotHash.Parse as P

parseKnotInput = P.parseKnotInput
computeKnotHash = C.hash 256
computeFullKnotHash = C.fullHash
showFullKnotHash = C.showFullHash