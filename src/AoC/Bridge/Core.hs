module AoC.Bridge.Core (
  MagComp, Bridge,
  makeMagComp, makeBridge,
  buildStrongestBridge, buildLongestBridge,
  computeStrength) where

import Data.List (delete, maximumBy, nub)

data MagComp = MagComp Int Int
  deriving (Eq)

data Bridge = Bridge [MagComp]
  deriving (Eq, Show)

makeBridge = Bridge
magComps (Bridge mcs) = mcs

instance Show MagComp where
  show (MagComp a b) = (show a) ++ ('/':(show b))

makeMagComp :: Int -> Int -> MagComp
makeMagComp = MagComp

computeStrength :: Bridge -> Int
computeStrength = sum . map strength . magComps where
  strength (MagComp a b) = a + b

bridgeLength = length . magComps

buildStrongestBridge :: [MagComp] -> Bridge
buildStrongestBridge = buildBridge cmp where
  cmp b b' = compare (computeStrength b) (computeStrength b')

buildBridge :: (Bridge -> Bridge -> Ordering) -> [MagComp] -> Bridge
buildBridge cmp = buildBridge' 0 where
  buildBridge' :: Int -> [MagComp] -> Bridge
  buildBridge' pins mcs = if null bridges then makeBridge [] else maximumBy cmp bridges where
    bridges = do
      mc <- candidates
      let mcs' = delete mc mcs
      let mc' = flipMagComp mc
      return $ appendMagComp mc $ buildBridge' (outputPins mc') mcs'
    inputPins (MagComp a _) = a
    outputPins (MagComp _ b) = b
    appendMagComp :: MagComp -> Bridge -> Bridge
    appendMagComp mc = makeBridge . ((:) mc) . magComps
    candidates = filter (\(MagComp a b) -> a == pins || b == pins) mcs
    flipMagComp (MagComp a b) = if a == pins then MagComp a b else MagComp b a

buildLongestBridge :: [MagComp] -> Bridge
buildLongestBridge = buildBridge cmp where
  cmp b b' = case compare (bridgeLength b) (bridgeLength b') of
    EQ  -> compare (computeStrength b) (computeStrength b')
    ord -> ord