module AoC.Bridge (
  buildStrongestBridge, buildLongestBridge,
  computeBridgeStrength,
  parseMagComps) where

import qualified AoC.Bridge.Core as C
import qualified AoC.Bridge.Parse as P

parseMagComps :: String -> Either String [C.MagComp]
parseMagComps = P.parseMagComps

buildStrongestBridge :: [C.MagComp] -> C.Bridge
buildStrongestBridge = C.buildStrongestBridge

buildLongestBridge :: [C.MagComp] -> C.Bridge
buildLongestBridge = C.buildLongestBridge

computeBridgeStrength :: C.Bridge -> Int
computeBridgeStrength = C.computeStrength