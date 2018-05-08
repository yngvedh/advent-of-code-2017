module AoC.SpinLock (
  emptySpinLock,
  parseSkipDistance,
  spinRight, currentValue,
  spin, spinN,
  simulateValueAtPos1) where

import qualified AoC.SpinLock.Parse as P
import qualified AoC.SpinLock.Core as C

emptySpinLock = C.emptySpinLock
parseSkipDistance = P.parseSkipDistance

spinRight = C.spinRight
currentValue = C.currentValue
spin = C.spin
spinN = C.spinN
simulateValueAtPos1 = C.simulateValueAtPos1
