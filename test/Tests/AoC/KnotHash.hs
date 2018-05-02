module Tests.AoC.KnotHash (describeKnotHash) where

import AoC.KnotHash.Core
import AoC.KnotHash.Parse

import Test.Hspec

import Data.Word
import Data.Char (ord)

describeKnotHash = describe "AoC.KnotHash" $ do
  describeKnotParse
  describeKnotComputeHash
  describeKnotShowFullHash
  describeKnotComputeFullHash
  describeShowBinary

describeKnotParse = describe "KnotHash parsing" $ do
  it "should parse sample correctly" $ do
    parseKnotInput "3,4,1,5\n" `shouldBe` Right ([3,4,1,5],[0x33,0x2c,0x34,0x2c,0x31,0x2c,0x35])

describeKnotComputeHash = describe "KnotHash hashing" $ do
  it "should compute sample correctly" $ do
    hash 5 [3,4,1,5] `shouldBe` 12

describeKnotShowFullHash = describe "KnotHash show full hash" $ do
  it "should produce the correct string" $ do
    showFullHash [0x12, 0x34, 0x56, 0x78, 0x9a, 0xbc, 0xde, 0xf0] `shouldBe` "123456789abcdef0"


describeKnotComputeFullHash = describe "KnotHash full hashing" $ do
  it "should hash '' correctly" $ do
    fullHash [] `shouldBe` [0xa2, 0x58, 0x2a, 0x3a, 0x0e, 0x66, 0xe6, 0xe8, 0x6e, 0x38, 0x12, 0xdc, 0xb6, 0x72, 0xa2, 0x72]

  it "should hash 'AoC 2017' correctly" $ do
    let lengths = codes "AoC 2017"
    fullHash lengths `shouldBe` [0x33, 0xef, 0xeb, 0x34, 0xea, 0x91, 0x90, 0x2b, 0xb2, 0xf5, 0x9c, 0x99, 0x20, 0xca, 0xa6, 0xcd]

  it "should hash '1,2,3' correctly" $ do
    let lengths = codes "1,2,3"
    fullHash lengths `shouldBe` [0x3e, 0xfb, 0xe7, 0x8a, 0x8d, 0x82, 0xf2, 0x99, 0x79, 0x03, 0x1a, 0x4a, 0xa0, 0xb1, 0x6a, 0x9d]

  it "should hash '1,2,4' correctly" $ do
    let lengths = codes "1,2,4"
    fullHash lengths `shouldBe` [0x63, 0x96, 0x08, 0x35, 0xbc, 0xdc, 0x13, 0x0f, 0x0b, 0x66, 0xd7, 0xff, 0x4f, 0x6a, 0x5a, 0x8e]

describeShowBinary = describe "KnotHash show binary hash" $ do
  it "should show correct bits for 0xa0c2017..." $ do
    let hash = [0xa0, 0xc2, 0x01, 0x70, 0x00, 0x00]
    showBinaryHash hash `shouldStartWith` "10100000110000100000000101110000"

