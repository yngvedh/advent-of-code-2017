module AoC.ParticleSwarm (parseParticles, findClosestLongTerm) where

import qualified AoC.ParticleSwarm.Core as C
import qualified AoC.ParticleSwarm.Parse as P

parseParticles = P.parseParticles
findClosestLongTerm = C.findClosestLongTerm