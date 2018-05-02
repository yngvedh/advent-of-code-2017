module Tests.AoC.Generator (describeGenerator) where

import AoC.Generator.Core
import AoC.Generator.Judge

import Test.Hspec

describeGenerator = describe "AoC.Generator" $ do
  
  let generatorA = makeSimpleGenerator 65 16807
  let generatorB = makeSimpleGenerator 8921 48271

  let filterGenA = makeFilterGenerator 65 4 16807
  let filterGenB = makeFilterGenerator 8921 8 48271

  describe "generateNext" $ do
    it "should generate a 'match' after three values." $ do
      let a = (flip mod 65536) . value . generateNext . generateNext $ generatorA
      let b = (flip mod 65536) . value . generateNext . generateNext $ generatorB
      a `shouldBe` b

  describe "isMatching" $ do
    it "should match after three values" $ do
      let a = generateNext . generateNext $ generatorA
      let b = generateNext . generateNext $ generatorB
      isMatching a b `shouldBe` True

  describe "simple generators" $ do
    it "should produce expected sequence of sample A" $ do
      let some = take 5 . generateAllValues $ generatorA
      some `shouldBe` [1092455, 1181022009, 245556042, 1744312007, 1352636452]

    it "should produce expected sequence of sample B" $ do
      let some = take 5 . generateAllValues $ generatorB
      some `shouldBe` [430625591, 1233683848, 1431495498, 137874439, 285222916]

  xdescribe "judgeGenerators" $ do
    it "should count the expected number of matches" $ do
      let matches = judgeGenerators 40000000 generatorA generatorB
      matches `shouldBe` 588

  describe "filter generators" $ do
    it "should produce expected sequence of sample A" $ do
      let some = take 5 . generateAllValues $ filterGenA
      some `shouldBe` [1352636452, 1992081072, 530830436, 1980017072, 740335192]

    it "should produce expected sequence of sample B" $ do
      let some = take 5 . generateAllValues $ filterGenB
      some `shouldBe` [1233683848, 862516352, 1159784568, 1616057672, 412269392]

  xdescribe "judgeGenerators" $ do
    it "should count the expected number of matches" $ do
      let matches = judgeGenerators 5000000 filterGenA filterGenB
      matches `shouldBe` 309

