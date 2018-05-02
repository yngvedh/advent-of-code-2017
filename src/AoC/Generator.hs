module AoC.Generator (
  parseGenerators, judgeGenerators,
  makeSimpleGenerator, makeFilterGenerator) where

import qualified AoC.Generator.Parse as P
import qualified AoC.Generator.Judge as J
import qualified AoC.Generator.Core as C

parseGenerators = P.parseGenerators
judgeGenerators = J.judgeGenerators
makeSimpleGenerator = C.makeSimpleGenerator
makeFilterGenerator = C.makeFilterGenerator