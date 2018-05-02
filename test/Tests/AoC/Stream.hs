module Tests.AoC.Stream (describeStreamParse, describeStreamScore, describeStreamCountGarbage) where

import AoC.Stream.Parse
import AoC.Stream.Core
import Test.Hspec

describeStreamParse = describe "AoC.Stream.Parse.parse" $ do
  it "Should parse '{}' correctly" $ do
    let expected = Right $ Group []
    parseStream "{}" `shouldBe` expected

  it "Should parse '{{{}}}' correctly." $ do
    let expected = Right $ Group [Group [Group []]]
    parseStream "{{{}}}" `shouldBe` expected

  it "Should parse '{{{},{},{{}}}}' correctly." $ do
    let expected = Right $ Group [Group [Group [], Group[], Group[Group []]]]
    parseStream "{{{},{},{{}}}}" `shouldBe` expected

  it "Should parse '{<{},{},{{}}>}' correctly" $ do
    let expected = Right $ Group [Garbage "{},{},{{}}"]
    parseStream "{<{},{},{{}}>}" `shouldBe` expected

  it "Should parse '{<a>,<a>,<a>,<a>}' correctly." $ do
    let expected = Right $ Group [Garbage "a", Garbage "a", Garbage "a", Garbage "a"]
    parseStream "{<a>,<a>,<a>,<a>}" `shouldBe` expected

  it "Should parse '{{<a>},{<a>},{<a>},{<a>}}' correctly." $ do
    let expected = Right $ Group [Group [Garbage "a"], Group[Garbage "a"], Group[Garbage "a"], Group[Garbage "a"]]
    parseStream "{{<a>},{<a>},{<a>},{<a>}}" `shouldBe` expected

  it "Should parse '{{<!>},{<!>},{<!>},{<a>}}' correctly" $ do
    let expected = Right $ Group [Group [Garbage "},{<},{<},{<a"]]
    parseStream "{{<!>},{<!>},{<!>},{<a>}}" `shouldBe` expected

describeStreamScore = describe "AoC.Stream.Core.score" $ do
  it "Should score '{}' correctly" $ do
    let group = Group []
    score group `shouldBe` 1

  it "Should score '{{{}}}' correctly." $ do
    let group = Group [Group [Group []]]
    score group `shouldBe` 6

  it "Should score '{{},{}}' correctly." $ do
    let group = Group [Group [], Group []]
    score group `shouldBe` 5

  it "Should score '{{{},{},{{}}}}' correctly" $ do
    let group = Group [Group [Group [], Group [], Group [Group []]]]
    score group `shouldBe` 16

  it "Should score'{<a>,<a>,<a>,<a>}' correctly." $ do
    let group = Group [Garbage "a", Garbage "a", Garbage "a", Garbage "a"]
    score group `shouldBe` 1

  it "Should parse '{{<ab>},{<ab>},{<ab>},{<ab>}}' correctly." $ do
    let group = Group [Group [Garbage "ab"], Group [Garbage "ab"], Group [Garbage "ab"], Group [Garbage "ab"]]
    score group `shouldBe` 9

  it "Should parse '{{<!!>},{<!!>},{<!!>},{<!!>}}' correctly." $ do
    let group = Group [Group [Garbage ""], Group [Garbage ""], Group [Garbage ""], Group [Garbage ""]]
    score group `shouldBe` 9

  it "Should parse '{{<a!>},{<a!>},{<a!>},{<ab>}}' correctly." $ do
    let group = Group [Group [Garbage "a!>},{<a!>},{<a!>},{<ab"]]
    score group `shouldBe` 3

describeStreamCountGarbage = describe "AoC.Stream.Core.countGarbage" $ do
  it "Should count <> correctly" $ do
    let (Right group) = parseStream "<>"
    countGarbage group `shouldBe` 0

  it "Should count <random characters> correctly" $ do
    let (Right group) = parseStream "<random characters>"
    countGarbage group `shouldBe` 17

  it "Should count <<<<> correctly" $ do
    let (Right group) = parseStream "<<<<>"
    countGarbage group `shouldBe` 3

  it "Should count <{!>}> correctly" $ do
    let (Right group) = parseStream "<{!>}>"
    countGarbage group `shouldBe` 2

  it "Should count <!!> correctly" $ do
    let (Right group) = parseStream "<!!>"
    countGarbage group `shouldBe` 0

  it "Should count <!!!>> correctly" $ do
    let (Right group) = parseStream "<!!!>>"
    countGarbage group `shouldBe` 0

  it "Should count <{o\"i!a,<{i<a> correctly" $ do
    let (Right group) = parseStream "<{o\"i!a,<{i<a>"
    countGarbage group `shouldBe` 10



