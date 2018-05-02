module Tests.AoC.Disk (describeDisk) where

import AoC.Disk.Core
import AoC.Focus.ListMatrix

import Data.Char (digitToInt)

import Test.Hspec

chrToCell :: Char -> Either Bool Int
chrToCell '.' = Left False
chrToCell '#' = Left True
chrToCell c = if c >= '0' && c <= '9' then Right (digitToInt c) else Left False

toCells = map (map chrToCell)

describeDisk = describe "AoC.Disk" $ do
  describe "diskFromKey and showDisk" $ do
    it "should generate and show the sample correctly" $ do
      let expected = ["##.#.#..",
                      ".#.#.#.#",
                      "....#.#.",
                      "#.#.##.#",
                      ".##.#...",
                      "##..#..#",
                      ".#...#..",
                      "##.#.##."]
      let disk = diskFromKey "flqrgnkx"
      let showed = lines . showDisk $ disk
      let excerpt = take 8 . map (take 8) $ showed
      excerpt `shouldBe` expected

  xdescribe "countRegions" $ do
    it "should count regions in sample correctly" $ do
      let disk = diskFromKey "flqrgnkx"
      countRegions disk `shouldBe` 1242

  describe "fillRegion" $ do
    it "should fill a completely filled disk correctly" $ do
      let focus = makeFocus $ toCells ["###","###","###"]
      let expected = toCells ["000", "000", "000"]
      unfocus (fillRegion 0 focus) `shouldBe` expected

    it "should fill a corner correctly" $ do
      let focus = makeFocus $ toCells ["##.","#.#",".##"]
      let expected = toCells ["00.","0.#",".##"]
      unfocus (fillRegion 0 focus) `shouldBe` expected

    it "should fill a donut correctly" $ do
      let focus = makeFocus $ toCells ["####","#..#","#..#", "####"]
      let expected = toCells ["0000","0..0", "0..0", "0000"]
      unfocus (fillRegion 0 focus) `shouldBe` expected

    it "should fill a cross correctly" $ do
      let focus = moveRight . makeFocus $ toCells [".#.","###",".#."]
      let expected = toCells [".0.","000",".0."]
      unfocus (fillRegion 0 focus) `shouldBe` expected

    it "should fill a cross correctly" $ do
      let focus = moveRight . makeFocus $ toCells ["###.","#.##",".##.","#.##"]
      let expected = toCells ["000.","0.00",".00.","#.00"]
      unfocus (fillRegion 0 focus) `shouldBe` expected
