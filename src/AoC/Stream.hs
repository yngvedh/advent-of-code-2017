module AoC.Stream (parseStream, scoreGroup, countGarbage) where

import qualified AoC.Stream.Parse as P
import qualified AoC.Stream.Core as C

parseStream = P.parseStream
scoreGroup = C.score
countGarbage = C.countGarbage