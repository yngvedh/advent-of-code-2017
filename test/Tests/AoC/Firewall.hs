module Tests.AoC.Firewall (describeFirewall) where

import AoC.Firewall.Core
import AoC.Firewall.Parse

import Test.Hspec

describeFirewall = describe "AoC.Firewall" $ do
  let sampleFirewall = Firewall [Layer 0 3, Layer 1 2, Layer 4 4, Layer 6 4]

  describe "Parsing" $ do
    it "should parse no layers correctly" $ do
      let expected = Right $ Firewall []
      parseFirewall "\n" `shouldBe` expected

    it "should parse one layer correctly" $ do
      let expected = Right $ Firewall [Layer 0 1]
      parseFirewall "0: 1\n" `shouldBe` expected

    it "should parse multiple layers correctly" $ do
      let expected = Right $ Firewall [Layer 1 1, Layer 2 2, Layer 4 6]
      parseFirewall "1: 1\n2: 2\n4: 6\n" `shouldBe` expected

  describe "Collision detection" $ do
    it "should find collision in layer 0" $ do
      let firewall = Firewall [Layer 0 10]
      detectCollisions firewall `shouldMatchList` [Layer 0 10]

    it "should not find collision in layer 1 x" $ do
      let firewall = Firewall [Layer 1 4]
      detectCollisions firewall `shouldMatchList` []

    it "should not find collision in layer x 1" $ do
      let firewall = Firewall [Layer 4 1]
      detectCollisions firewall `shouldMatchList` [Layer 4 1]

    it "should find collisions in sample" $ do
      detectCollisions sampleFirewall `shouldMatchList` [Layer 0 3, Layer 6 4]

  describe "Collision score" $ do
    it "should score sample collisions correctly" $ do
      let collisions = [Layer 0 3, Layer 6 4]
      scoreCollisions collisions `shouldBe` 24

  describe "Find delay" $ do
    it "should find correct delay for sample" $ do
      undetectableDelay sampleFirewall `shouldBe` 10
