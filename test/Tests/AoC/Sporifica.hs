module Tests.AoC.Sporifica (describeSporifica) where

import AoC.Sporifica.Core
import AoC.Sporifica.Parse
import AoC.Sporifica.Simulation

import Test.Hspec

describeSporifica = describe "AoC.Sporifica" $ do
  let sampleGrid = makeGridFromPositions [Pos (-1) 0, Pos 1 1]

  describe "parseNodeGrid" $ do
    it "should parse sample correctly" $ do
      let sampleInput = unlines
                ["..#",
                 "#..",
                 "..."]
      parseGrid sampleInput `shouldBe` Right sampleGrid

    it "should parse bigger sample correctly" $ do
      let sampleInput = unlines
                ["#..#.",
                 "...#.",
                 "....#",
                 "#....",
                 "#...#"]
      let expected = makeGridFromPositions
                [Pos (-2) 2, Pos 1 2,
                 Pos 1 1,
                 Pos 2 0,
                 Pos (-2) (-1),
                 Pos (-2) (-2), Pos 2 (-2)]
      parseGrid sampleInput `shouldBe` Right expected

  describe "stepCarrier" $ do
    it "makes step 0 of the example correctly" $ do
      let grid = makeGridFromPositions [Pos (-1) 0, Pos 1 1]
      let carrier = Carrier (Pos 0 0) (Vec 0 1)
      let grid' = makeGridFromPositions [Pos (-1) 0, Pos 1 1, Pos 0 0]
      let carrier' = Carrier (Pos (-1) 0) (Vec (-1) 0)
      stepCarrier (SimulationState carrier grid) `shouldBe` (SimulationState carrier' grid')

    it "makes step 1 of the example correctly" $ do
      let grid = makeGridFromPositions [Pos (-1) 0, Pos 1 1, Pos 0 0]
      let carrier = Carrier (Pos (-1) 0) (Vec (-1) 0)
      let grid' = makeGridFromPositions [Pos 1 1, Pos 0 0]
      let carrier' = Carrier (Pos (-1) 1) (Vec 0 1)
      stepCarrier (SimulationState carrier grid) `shouldBe` (SimulationState carrier' grid')
      
  describe "runSimulation" $ do
    it "counts infected nodes correctly in sample (n=70)" $ do
      runSimulation 70 sampleGrid `shouldBe` SimulationResult { infected = 41 }
    
    it "counts infected nodes correctly in sample (n=10000)" $ do
      runSimulation 10000 sampleGrid `shouldBe` SimulationResult { infected = 5587 }