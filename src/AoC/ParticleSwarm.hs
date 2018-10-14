module AoC.ParticleSwarm (parseParticles, findClosestLongTerm, findRemainingParticles) where

import qualified AoC.ParticleSwarm.Core as C
import qualified AoC.ParticleSwarm.Parse as P

parseParticles = P.parseParticles
findClosestLongTerm = C.findClosestLongTerm
findRemainingParticles = C.findRemainingAfterCollisions