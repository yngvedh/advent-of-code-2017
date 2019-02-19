module Tests.AoC.Bridge (describeBridge) where

import AoC.Bridge.Core
import AoC.Bridge.Parse

import Test.Hspec

describeBridge = describe "AoC.Bridge" $ do
  let sampleMagComps = [makeMagComp 0 2, makeMagComp 2 2, makeMagComp 2 3, makeMagComp 3 4, makeMagComp 3 5, makeMagComp 0 1, makeMagComp 10 1, makeMagComp 9 10]
  let solution = makeBridge [makeMagComp 0 1, makeMagComp 10 1, makeMagComp 9 10]
  let solution2 = makeBridge [makeMagComp 0 2, makeMagComp 2 2, makeMagComp 2 3, makeMagComp 3 5]
  describe "parse" $ do
    it "should parse sample input correctly" $ do
      let sampleInput = "0/2\n2/2\n2/3\n3/4\n3/5\n0/1\n10/1\n9/10\n"
      parseMagComps sampleInput `shouldBe` Right sampleMagComps
    
  describe "solve" $ do
    it "should find the strongest bridge in the sample" $ do
      buildStrongestBridge sampleMagComps `shouldBe` solution

    it "should find the longest, strongest bridge in the sample" $ do
      buildLongestBridge sampleMagComps `shouldBe` solution2

  describe "computeStrength" $ do
    it "should computeStrength solution correctly" $ do
      computeStrength solution `shouldBe` 31
