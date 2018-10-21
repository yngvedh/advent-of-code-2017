module Tests.AoC.Sporifica (describeSporifica) where

import AoC.Sporifica.Core
import AoC.Sporifica.Parse

import Test.Hspec

describeSporifica = describe "AoC.Sporifica" $ do
  let sampleGrid = makeGrid
                    [[Clean,Clean,Infected],
                     [Infected,Clean,Clean],
                     [Clean,Clean,Clean]]

  describe "parseNodeGrid" $ do
    it "should parse sample correctly" $ do
      let sampleInput = unlines
                ["..#",
                 "#..",
                 "..."]
      parseGrid sampleInput `shouldBe` Right sampleGrid
  