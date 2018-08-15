module Tests.AoC.Duet (describeDuet) where

import AoC.Duet.Core
import AoC.Duet.Solo
import AoC.Duet.Parse
import AoC.Focus.List

import Test.Hspec

testExecutionContext is rs ch pos = ExecutionContext (Cpu rs' p) ch where
  rs' = Registers $ map (\(a,b) -> (Register a, b)) rs
  p = Program $ makeFocusAt pos is

describeDuet = describe "AoC.Duet" $ do
  let sampleInput = "set a 1\nadd a 2\nmul a a\nmod a 5\nsnd a\nset a 0\nrcv a\njgz a -1\nset a 1\njgz a -2\n"
  let sampleProgram = [Set (Register "a") (LiteralValue 1),
                       Add (Register "a") (LiteralValue 2),
                       Mul (Register "a") (RegisterValue $ Register "a"),
                       Mod (Register "a") (LiteralValue 5),
                       Snd (RegisterValue $ Register "a"),
                       Set (Register "a") (LiteralValue 0),
                       Rcv (Register "a"),
                       Jgz (Register "a") (LiteralValue (-1)),
                       Set (Register "a") (LiteralValue 1),
                       Jgz (Register "a") (LiteralValue (-2))]
  let testEC rs hz = testExecutionContext sampleProgram rs (IoReg hz)

  describe "parse" $
    it "should parse sample correctly" $
      parseInstructions sampleInput `shouldBe` Right sampleProgram

  describe "stepOnce" $ do
    it "should run example and produce correct state and intermediate states" $ do
      let expecteds = [testEC [] 0 0,
                      testEC [("a", 1)] 0 1,
                      testEC [("a", 3)] 0 2,
                      testEC [("a", 9)] 0 3,
                      testEC [("a", 4)] 0 4,
                      testEC [("a", 4)] 4 5,
                      testEC [("a", 0)] 4 6,
                      testEC [("a", 0)] 4 7,
                      testEC [("a", 0)] 4 8,
                      testEC [("a", 1)] 4 9,
                      testEC [("a", 1)] 4 7,
                      testEC [("a", 1)] 4 6,
                      testEC [("a", 4)] 4 7]

      let results = take (length expecteds) . iterate stepOnce . emptyExecutionContext $ sampleProgram
      mconcat $ zipWith shouldBe results expecteds

    it "Should read,write buffer correctly" $ do
      let testProgram = [Snd $ LiteralValue 1,
                        Snd $ LiteralValue 2,
                        Snd $ LiteralValue 3,
                        Set (Register "a") (LiteralValue 7),
                        Rcv $ Register "a",
                        Rcv $ Register "a",
                        Rcv $ Register "a",
                        Set (Register "a") (LiteralValue 0)]

      let testEC rs hzs = testExecutionContext testProgram rs (IoBuf hzs)

      let expecteds = [testEC [] [] 0,
                       testEC [] [1] 1,
                       testEC [] [2,1] 2,
                       testEC [] [3,2,1] 3,
                       testEC [("a", 7)] [3,2,1] 4,
                       testEC [("a", 3)] [2,1] 5,
                       testEC [("a", 2)] [1] 6,
                       testEC [("a", 1)] [] 7]
      
      let results = take (length expecteds) . iterate stepOnce . emptyExecutionContext $ testProgram
      mconcat $ zipWith shouldBe results expecteds

  describe "runSolo" $
    it "should run example and produce correct state" $
      runSoloFirstRcv (emptyExecutionContext sampleProgram) `shouldBe` testEC [("a", 4)] 4 7


