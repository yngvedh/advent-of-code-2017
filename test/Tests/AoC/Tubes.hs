module Tests.AoC.Tubes (describeTubes) where

import Test.Hspec
import AoC.Tubes.Parse
import AoC.Tubes.Core

describeTubes = describe "AoC.Tubes" $ do
  let examplePuzzle = makeTubes [[Space,Space,Space,Space,Vertical,Space,Space,Space,Space,Space,Space,Space,Space,Space,Space],
                                 [Space,Space,Space,Space,Vertical,Space,Space,Cross,Horizontal,Horizontal,Cross,Space,Space,Space,Space],
                                 [Space,Space,Space,Space,Letter 'A',Space,Space,Vertical,Space,Space,Letter 'C',Space,Space,Space,Space],
                                 [Letter 'F',Horizontal,Horizontal,Horizontal,Vertical,Horizontal,Horizontal,Horizontal,Horizontal,Letter 'E',Vertical,Horizontal,Horizontal,Cross,Space],
                                 [Space,Space,Space,Space,Vertical,Space,Space,Vertical,Space,Space,Vertical,Space,Space,Letter 'D',Space],
                                 [Space,Space,Space,Space,Cross,Letter 'B',Horizontal,Cross,Space,Space,Cross,Horizontal,Horizontal,Cross,Space]]

  describe "parse" $ do
    it "should parse example correctly" $ do
      let exampleInput = unlines ["    |          ",
                                  "    |  +--+    ",
                                  "    A  |  C    ",
                                  "F---|----E|--+ ",
                                  "    |  |  |  D ",
                                  "    +B-+  +--+ ",
                                  ""]
      parseTubes exampleInput `shouldBe` Right examplePuzzle
    
    it "should start at the correct position" $ do
      let exampleInput = unlines [" | ", " +-"]
      let expected = makeTubesAt 1 0 [[Space, Vertical, Space], [Space, Cross, Horizontal]]
      parseTubes exampleInput `shouldBe` Right  expected
  
  describe "run" $ do
    it "should record a single letter" $ do
      let examplePuzzle = makeTubes [[Space, Vertical, Space],[Space, Letter 'G', Space],[Space, Vertical, Space]]
      runAndRecordLetters examplePuzzle `shouldBe` "G"
    
    it "should record letters in the right order" $ do
      let examplePuzzle = makeTubes [[Space,Vertical,Space],[Space,Letter 'F', Space],[Space,Cross, Letter 'U'], [Space, Space, Space]]
      runAndRecordLetters examplePuzzle `shouldBe` "FU"
    
    it "should record letters of sample correctly" $ do
      runAndRecordLetters examplePuzzle `shouldBe` "ABCDEF"

    it "should count the number of steps correctly" $ do
      runAndRecordSteps examplePuzzle `shouldBe` 38
