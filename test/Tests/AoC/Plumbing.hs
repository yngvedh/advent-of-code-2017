module Tests.AoC.Plumbing (describePlumbing) where

import Test.Hspec
import Data.List (nub, find)
import Data.Maybe (fromJust)

import AoC.Plumbing.Parse
import AoC.Plumbing.Core

{-- Sample input:
0 <-> 2
1 <-> 1
2 <-> 0, 3, 4
3 <-> 2, 4
4 <-> 2, 3, 6
5 <-> 6
6 <-> 4, 5
--}
makeSampleVillage :: [[Int]] -> Village
makeSampleVillage = Village . nub . concat . make' 0 where
  make' :: Int -> [[Int]] -> [[(Program, Program)]]
  make' _ [] = []
  make' id (p:ps) = (concat . map (\c -> [(Program id, Program c), (Program c, Program id)]) $ p):(make' (id+1) ps)

describePlumbing = describe "AoC.Plumbing" $ do
  let sampleVillage = makeSampleVillage [[2],[1],[0,3,4],[2,4],[2,3,6],[6],[4,5]]

  describe "parse, getPrograms and getConnected" $ do
    it "should parse sample input correctly" $ do
      let (Right parsed) = parsePlumbing "0 <-> 2\n1 <-> 1\n2 <-> 0, 3, 4\n3 <-> 2, 4\n4 <-> 2, 3, 6\n5 <-> 6\n6 <-> 4, 5\n"
      getPrograms parsed `shouldMatchList` (map Program [0..6])
      getConnected parsed (Program 0) `shouldMatchList` map Program [2]
      getConnected parsed (Program 1) `shouldMatchList` map Program [1]
      getConnected parsed (Program 2) `shouldMatchList` map Program [0, 3, 4]
      getConnected parsed (Program 3) `shouldMatchList` map Program [2, 4]
      getConnected parsed (Program 4) `shouldMatchList` map Program [2, 3, 6]
      getConnected parsed (Program 5) `shouldMatchList` map Program [6]
      getConnected parsed (Program 6) `shouldMatchList` map Program [4, 5]

  describe "getGroup" $ do
    it "should group program 0 of the sample correctly" $ do
      getGroup sampleVillage (Program 0) `shouldMatchList` map Program [0,2,3,4,5,6]

  describe "getGroups" $ do
    it "should detect groups of the sample correctly" $ do
      let groups = getGroups sampleVillage
      length groups `shouldBe` 2
      (fromJust . find (elem $ Program 0) $ groups) `shouldMatchList` map Program [0,2,3,4,5,6]
      (fromJust . find (elem $ Program 1) $ groups) `shouldMatchList` map Program [1]


