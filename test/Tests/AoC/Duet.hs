module Tests.AoC.Duet (describeDuet) where

import Data.List (inits)

import AoC.Duet.Core
import AoC.Duet.Solo
import AoC.Duet.Channel
import qualified AoC.Duet.Duo as D
import AoC.Duet.Parse
import AoC.Focus.List

import Test.Hspec

fromRight (Right v) = v

testExecutionContext is rs ch pos = ExecutionContext (Cpu rs' p) ch makeExecutionLog where
  rs' = makeRegistersFromList $ map (\(a,b) -> (Register a, b)) rs
  p = Program $ makeFocusAt pos is

numReads, numWrites :: ExecutionLog -> Int
numReads (ExecutionLog is) = length . filter isRcv $ is where
  isRcv (Exe (Rcv _)) = True
  isRcv _ = False
numWrites (ExecutionLog is) = length . filter isSnd $ is where
  isSnd (Exe (Snd _)) = True
  isSnd _ = False

testDuoExecutionContext :: (Channel a) => (b -> a) -> [Instruction] -> [(String, Int)] -> b -> Int -> [(String,Int)] -> b -> Int -> D.ExecutionContext a
testDuoExecutionContext mkCh is rs ch pos rs' ch' pos' = D.ExecutionContext ("0", testExecutionContext is rs (mkCh ch) pos) ("1", testExecutionContext is rs' (mkCh ch') pos')

compositions :: [a->a] -> [a->a]
compositions = map (foldl1 (.)) . drop 1 . inits

describeDuet = describe "AoC.Duet" $ do
  let sampleInput = "set a 1\nadd a 2\nmul a a\nmod a 5\nsnd a\nset a 0\nrcv a\njgz a -1\nset a 1\njgz a -2\n"
  let sampleProgram = [Set (Register "a") (LiteralValue 1),
                       Add (Register "a") (LiteralValue 2),
                       Mul (Register "a") (RegisterValue $ Register "a"),
                       Mod (Register "a") (LiteralValue 5),
                       Snd (RegisterValue $ Register "a"),
                       Set (Register "a") (LiteralValue 0),
                       Rcv (Register "a"),
                       Jgz (RegisterValue . Register $ "a") (LiteralValue (-1)),
                       Set (Register "a") (LiteralValue 1),
                       Jgz (RegisterValue . Register $ "a") (LiteralValue (-2))]
  let testEC rs hz = testExecutionContext sampleProgram rs (IoReg hz)

  describe "parse" $ do
    it "should parse sample correctly" $
      parseInstructions sampleInput `shouldBe` Right sampleProgram

    it "should parse all of puzzle input" $ do
      let puzzleInput = "set i 31\nset a 1\nmul p 17\njgz p p\nmul a 2\nadd i -1\njgz i -2\nadd a -1\nset i 127\nset p 618\nmul p 8505\nmod p a\nmul p 129749\nadd p 12345\nmod p a\nset b p\nmod b 10000\nsnd b\nadd i -1\njgz i -9\njgz a 3\nrcv b\njgz b -1\nset f 0\nset i 126\nrcv a\nrcv b\nset p a\nmul p -1\nadd p b\njgz p 4\nsnd a\nset a b\njgz 1 3\nsnd b\nset f 1\nadd i -1\njgz i -11\nsnd a\njgz f -16\njgz a -19\n"
      let (Right parsed) = parseInstructions puzzleInput
      length parsed `shouldBe` 41

    it "should parse jgz with literal as first parameter correctly" $ do
      let input = "jgz 1 a\n"
      let expected = Right [Jgz (LiteralValue 1) (RegisterValue $ Register "a")]
      parseInstructions input `shouldBe` expected

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

      let results = take (length expecteds) . iterate (fromRight . stepOnce) . emptyExecutionContext $ sampleProgram
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
                       testEC [("a", 1)] [3,2] 5,
                       testEC [("a", 2)] [3] 6,
                       testEC [("a", 3)] [] 7]

      let results = take (length expecteds) . iterate (fromRight . stepOnce) . emptyExecutionContext $ testProgram
      results `shouldBe` expecteds

  describe "runSolo" $
    it "should run example and produce correct state" $ do
      let expected = Right $ testEC [("a", 4)] 4 7
      runSoloFirstRcv (emptyExecutionContext sampleProgram) `shouldBe` expected

  describe "duo" $ do
    let sampleProgram = [Snd $ LiteralValue 1,
                         Snd $ LiteralValue 2,
                         Snd . RegisterValue . Register $ "a",
                         Rcv $ Register "a",
                         Rcv $ Register "b",
                         Rcv $ Register "c",
                         Rcv $ Register "d"]

    describe "stepOnce" $ do
      it "should send data to the second solo ec" $ do
        let testProgram = [Snd $ LiteralValue 4, Rcv $ Register "a"]
        let testEc = testDuoExecutionContext IoBuf testProgram
        let ec = D.emptyExecutionContext testProgram :: D.ExecutionContext IoBuf
        let expected = testEc [("a", 0)] [] 1 [("a", 1)] [4] 0
        D.stepOnce ec `shouldBe` Right expected

      it "should swap register 'p' with each other" $ do
        let testProgram = [Snd $ RegisterValue (Register "p"), Set (Register "p") (LiteralValue 3), Rcv $ Register "p", Rcv $ Register "p"]
        let testEc = testDuoExecutionContext IoBuf testProgram
        let ec = D.emptyExecutionContext testProgram :: D.ExecutionContext IoBuf
        let s0 = fromRight . D.stepOnce
        let s1 = \ec -> fromRight $ Right ec >>= D.swapCpus >>= D.stepOnce >>= D.swapCpus
        let ss = compositions $ cycle [s0, s1]
        let ecs = map (\ x -> x ec) ss
        let expecteds = [testEc [("p", 0)] [] 1 [("p", 1)] [0] 0,
                         testEc [("p", 0)] [1] 1 [("p", 1)] [0] 1,
                         testEc [("p", 3)] [1] 2 [("p", 1)] [0] 1,
                         testEc [("p", 3)] [1] 2 [("p", 3)] [0] 2,
                         testEc [("p", 1)] [] 3 [("p", 3)] [0] 2,
                         testEc [("p", 1)] [] 3 [("p", 0)] [] 3]
        take (length expecteds) ecs `shouldBe` expecteds

      it "should step through sample correctly" $ do
        let testEc = testDuoExecutionContext IoBuf sampleProgram
        let ec = D.emptyExecutionContext sampleProgram :: D.ExecutionContext IoBuf
        let step = fromRight . D.stepDuoOnce where
        let ecs = iterate step ec
        let expecteds = []
        take (length expecteds) ecs `shouldBe` expecteds

    describe "run" $ do
      it "should not run if waiting" $ do
        let testProgram = [Rcv (Register "a")]
        let testEc = testDuoExecutionContext IoBuf testProgram
        let ec = testEc [("a", 1)] [] 0 [("a", 1)] [] 0
        D.run ec `shouldBe` Right ec

      it "both solos should swap values" $ do
        let testProgram = [Snd $ RegisterValue $ Register "a", Rcv $ Register "a", Rcv $ Register "a"]
        let testEc = testDuoExecutionContext IoBuf testProgram
        let expected = testEc [("a", 3)] [] 2 [("a", 2)] [] 2
        let ec = testEc [("a", 2)] [] 0 [("a", 3)] [] 0
        let (Right ec') = D.run ec
        let l0 = executionLog . D.soloByName "0" $ ec'
        let l1 = executionLog . D.soloByName "1" $ ec'
        numReads l0 `shouldBe` 1
        numReads l1 `shouldBe` 1
        numWrites l0 `shouldBe` 1
        numWrites l1 `shouldBe` 1
        ec' `shouldBe` expected

      it "should run sample correctly" $ do
        let ec = D.emptyExecutionContext sampleProgram :: D.ExecutionContext IoBuf
        let (Right ec') = D.run ec
        let l = executionLog . D.soloByName "1" $ ec'
        numWrites l `shouldBe` 3


    describe "Jgz" $ do
      let makeProgram offset = [Rcv (Register "a"),
                                Jgz (RegisterValue (Register "a")) (LiteralValue offset),
                                Rcv (Register "a"),
                                Rcv (Register "a"),
                                Rcv (Register "a"),
                                Rcv (Register "a"),
                                Rcv (Register "a")]

      it "Should jump to previous instruction with offset -1" $ do
        let testEc = testDuoExecutionContext IoBuf (makeProgram (-1))
        let ec = testEc [("a", 1)] [] 1 [] [] 0
        let expected = testEc [("a", 1)] [] 0 [] [] 0
        let ec' = D.stepOnce ec
        ec' `shouldBe` Right expected

      it "Should skip next instruction with offset 2" $ do
        let testEc = testDuoExecutionContext IoBuf (makeProgram 2)
        let ec = testEc [("a", 1)] [] 1 [] [] 0
        let expected = testEc [("a", 1)] [] 3 [] [] 0
        let ec' = D.stepOnce ec
        ec' `shouldBe` Right expected

      it "Should skip next 3 instructions with offset 4" $ do
        let testEc = testDuoExecutionContext IoBuf (makeProgram 4)
        let ec = testEc [("a", 1)] [] 1 [] [] 0
        let expected = testEc [("a", 1)] [] 5 [] [] 0
        let ec' = D.stepOnce ec
        ec' `shouldBe` Right expected

      it "Should jump to next instruction if register is <= 0" $ do
        let testEc = testDuoExecutionContext IoBuf (makeProgram 4)
        let ec = testEc [("a", 0)] [] 1 [] [] 0
        let expected = testEc [("a", 0)] [] 2 [] [] 0
        let ec' = D.stepOnce ec
        ec' `shouldBe` Right expected

      it "Should jump if parameter is literal > 0" $ do
        let testProgram = [Jgz (LiteralValue 5) (LiteralValue 0), Rcv (Register "a")]
        let testEc = testDuoExecutionContext IoBuf testProgram
        let ec = testEc [] [] 0 [] [] 0
        let expected = testEc [] [] 0 [] [] 0
        let ec' = D.stepOnce ec
        ec' `shouldBe` Right expected

      it "Should not jump if parameter is literal <= 0" $ do
        let testProgram = [Jgz (LiteralValue 0) (LiteralValue 0), Rcv (Register "a")]
        let testEc = testDuoExecutionContext IoBuf testProgram
        let ec = testEc [] [] 0 [] [] 0
        let expected = testEc [] [] 1 [] [] 0
        let ec' = D.stepOnce ec
        ec' `shouldBe` Right expected





