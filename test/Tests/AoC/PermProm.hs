module Tests.AoC.PermProm (describePermProm) where

import AoC.PermProm.Core
import AoC.PermProm.Parse

import Test.Hspec

describePermProm = describe "AoC.PermProm" $ do
  let dancers = map Dancer ['a'..'e']

  describe "parser" $ do
    it "should parse spin correctly" $ do
      parseDanceMoves "s1\n" `shouldBe` Right [Spin 1]
      parseDanceMoves "s64\n" `shouldBe` Right [Spin 64]

    it "should parse exchange correctly" $ do
      parseDanceMoves "x1/2\n" `shouldBe` Right [Exchange 1 2]
      parseDanceMoves "x99/0\n" `shouldBe` Right [Exchange 99 0]

    it "should parse partner correctly" $ do
      parseDanceMoves "pa/b\n" `shouldBe` Right [Partner (Dancer 'a') (Dancer 'b')]
      parseDanceMoves "pz/r\n" `shouldBe` Right [Partner (Dancer 'z') (Dancer 'r')]

    it "should parse multiple dance moves" $ do
      parseDanceMoves "s1,x1/2,pa/b\n" `shouldBe` Right [Spin 1, Exchange 1 2, Partner (Dancer 'a') (Dancer 'b')]

  describe "spin" $ do
    it "should not change anything for spin 0" $ do
      spin 0 dancers `shouldBe` dancers

    it "should not change anything for spin == #dancers" $ do
      spin 5 dancers `shouldBe` dancers

    it "should move back dancer to front on spin 1" $ do
      spin 1 dancers `shouldBe` map Dancer "eabcd"

    it "should front dancer to front on spin (#dancers-1)" $ do
      spin 4 dancers `shouldBe` map Dancer "bcdea"

    it "should match sample" $ do
      spin 3 dancers `shouldBe` map Dancer "cdeab"

  describe "exchange" $ do
    it "should not change anything for swap x x" $ do
      exchange 3 3 dancers `shouldBe` dancers

    it "should swap first and last correctly" $ do
      exchange 0 4 dancers `shouldBe` map Dancer "ebcda"

    it "should swap other elemeents correctly" $ do
      exchange 2 3 dancers `shouldBe` map Dancer "abdce"

    it "should swap element correctly when i > j" $ do
      exchange 4 2 dancers `shouldBe` map Dancer "abedc"

  describe "partner" $ do
    it "should not change anything for partner x x" $ do
      partner (Dancer 'b') (Dancer 'b') dancers `shouldBe` dancers

    it "should swap first and last correctly" $ do
      partner (Dancer 'a') (Dancer 'e') dancers `shouldBe` map Dancer "ebcda"

    it "should swap other elements correctly" $ do
      partner (Dancer 'c') (Dancer 'd') dancers `shouldBe` map Dancer "abdce"

    it "should swap if a is after b in line" $ do
      partner (Dancer 'e') (Dancer 'c') dancers `shouldBe` map Dancer "abedc"

  describe "dance" $ do
    it "should execute sample correctly" $ do
      let moves = [Spin 1, Exchange 3 4, Partner (Dancer 'e') (Dancer 'b')]
      dance moves dancers `shouldBe` map Dancer "baedc"

