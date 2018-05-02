module Tests.AoC.HexGrid (describeHexGrid) where

import AoC.HexGrid.Parse
import AoC.HexGrid.Core

import Test.Hspec

{- examples:
ne,ne,ne is 3 steps away.
ne,ne,sw,sw is 0 steps away (back where you started).
ne,ne,s,s is 2 steps away (se,se).
se,sw,se,sw,sw is 3 steps away (s,s,sw).
-}

describeHexGrid = describe "AoC.HexGrid" $ do
  describe "parse" $ do
    it "should parse 'ne,ne,ne' correctly" $ do
      let expected = Right $ HexPath [NE,NE,NE]
      parsePath "ne,ne,ne\n" `shouldBe` expected

    it "should parse 'ne,ne,sw,sw' correctly" $ do
      let expected = Right $ HexPath [NE,NE,SW,SW]
      parsePath "ne,ne,sw,sw\n" `shouldBe` expected

    it "should parse 'ne,ne,s,s' correctly" $ do
      let expected = Right $ HexPath [NE,NE,S,S]
      parsePath "ne,ne,s,s\n" `shouldBe` expected

    it "should parse 'se,sw,se,sw,sw' correctly" $ do
      let expected = Right $ HexPath [SE,SW,SE,SW,SW]
      parsePath "se,sw,se,sw,sw\n" `shouldBe` expected

    it "should parse 'n,nw,ne,s,sw,se' correctly" $ do
      let expected = Right $ HexPath [N,NW,NE,S,SW,SE]
      parsePath "n,nw,ne,s,sw,se\n" `shouldBe` expected

  describe "endPos" $ do
    it "should compute 'ne,ne,ne' correctly" $ do
      endPos (HexPath [NE,NE,NE]) `shouldBe` HexPos 3 3

    it "should compute 'ne,ne,sw,sw' correctly" $ do
      endPos (HexPath [NE,NE,SW,SW]) `shouldBe` HexPos 0 0

    it "should compute 'ne,ne,s,s' correctly" $ do
      endPos (HexPath [NE,NE,S,S]) `shouldBe` HexPos 0 2

    it "should compute 'se,sw,se,sw,sw' correctly" $ do
      endPos (HexPath [SE,SW,SE,SW,SW]) `shouldBe` HexPos (-3) (-1)

    it "should compute 'n,nw,ne,s,sw,se' correctly" $ do
      endPos (HexPath [N,NW,NE,S,SW,SE]) `shouldBe` HexPos 0 0

  describe "hexManhattanDist" $ do
    it "should compute '3,3' correctly" $ do
      hexManhattanDist (HexPos 3 3) `shouldBe` 3

    it "should compute '0,0' correctly" $ do
      hexManhattanDist (HexPos 0 0) `shouldBe` 0

    it "should compute '0,2' correctly" $ do
      hexManhattanDist (HexPos 0 2) `shouldBe` 2

    it "should compute '-3,1' correctly" $ do
      hexManhattanDist (HexPos (-3) (-1)) `shouldBe` 3

  describe "pathPositions" $ do
    it "should compute 'ne,ne,ne' correctly" $ do
      pathPositions (HexPath [NE,NE,NE]) `shouldBe` [HexPos 1 1, HexPos 2 2, HexPos 3 3]

    it "should compute 'ne,ne,s,s' correctly" $ do
      pathPositions (HexPath [NE,NE,S,S]) `shouldBe` [HexPos 1 1, HexPos 2 2, HexPos 1 2, HexPos 0 2]

    it "should compute 'se,sw,se,sw,sw' correctly" $ do
      pathPositions (HexPath [SE,SW,SE,SW,SW]) `shouldBe` [HexPos 0 1, HexPos (-1) 0, HexPos (-1) 1, HexPos (-2) 0, HexPos (-3) (-1)]

    it "should compute 'n,nw,ne,s,sw,se' correctly" $ do
      pathPositions (HexPath [N,NW,NE,S,SW,SE]) `shouldBe` [HexPos 1 0, HexPos 1 (-1), HexPos 2 0, HexPos 1 0, HexPos 0 (-1), HexPos 0 0]
