import Test.Hspec

import Tests.AoC.Stream
import Tests.AoC.KnotHash
import Tests.AoC.HexGrid
import Tests.AoC.Plumbing
import Tests.AoC.Firewall
import Tests.AoC.Disk
import Tests.AoC.Focus
import Tests.AoC.Generator
import Tests.AoC.PermProm
import Tests.AoC.SpinLock
import Tests.AoC.Duet
import Tests.AoC.Tubes
import Tests.AoC.ParticleSwarm
import Tests.AoC.OldTests

main :: IO ()
main = hspec $ do
  describeOldTests
  describeStreamParse
  describeStreamScore
  describeStreamCountGarbage
  describeKnotHash
  describeHexGrid
  describePlumbing
  describeFirewall
  describeDisk
  describeFocus
  describeGenerator
  describePermProm
  describeSpinLock
  describeDuet
  describeTubes
  describeParticleSwarm
