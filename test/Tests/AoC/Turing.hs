module Tests.AoC.Turing (describeTuring) where

import AoC.Turing.Parse
import AoC.Turing.Core

import Test.Hspec

makeTestSetup :: String -> Int -> [(String, (TapeValue, Movement, String), (TapeValue, Movement, String))] -> TuringSetup
makeTestSetup name n ss = makeSetup (Name name) n $ makeTestStates ss

makeTestStates :: [(String, (TapeValue, Movement, String), (TapeValue, Movement, String))] -> TuringStates
makeTestStates ss = makeStates $ map makeTestState ss

makeTestState :: (String, (TapeValue, Movement, String), (TapeValue, Movement, String)) -> (Name, TuringState)
makeTestState (name, a, b) = (Name name, makeState (makeTestAction a) (makeTestAction b))

makeTestAction :: (TapeValue, Movement, String) -> TuringAction
makeTestAction (x,y,z) = makeAction x y (Name z)

describeTuring = describe "AoC.Turing" $ do
  let testSetup n = makeTestSetup "A" n [("A", (Set, MoveRight, "B"), (Reset, MoveLeft,  "B")),
                                         ("B", (Set, MoveLeft,  "A"), (Set,   MoveRight, "A"))]
  let sampleSetup = testSetup 6
  describe "when parsing sample input" $ do
    it "should produce expected setup" $ do
      let input = "Begin in state A.\nPerform a diagnostic checksum after 6 steps.\nIn state A:\n  If the current value is 0:\n    - Write the value 1.\n    - Move one slot to the right.\n    - Continue with state B.\n  If the current value is 1:\n    - Write the value 0.\n    - Move one slot to the left.\n    - Continue with state B.\nIn state B:\n  If the current value is 0:\n    - Write the value 1.\n    - Move one slot to the left.\n    - Continue with state A.\n  If the current value is 1:\n    - Write the value 1.\n    - Move one slot to the right.\n    - Continue with state A.\n"
      parseTuringSetup input `shouldBe` Right sampleSetup

  describe "when running sample" $ do
    let sampleMachine = makeMachine sampleSetup
    let result = runTuring sampleMachine
  
    it "should produce the correct tape from sample" $ do
      tapeTailLeft result `shouldBe` [Set, Set]
      tapeTailRight result `shouldBe` [Set]
      currentValue result `shouldBe` Reset
      currentState result `shouldBe` (Name "A")

    it "should produce correct checksum from sample" $ do
      turingChecksum result `shouldBe` 3

  describe "when running partial sample" $ do
    it "should produce the correct tape from sample @N=1" $ do
      let result = runTuring . makeMachine $ testSetup 1
      tapeTailLeft result `shouldBe` [Set]
      tapeTailRight result `shouldBe` []
      currentValue result `shouldBe` Reset
      currentState result `shouldBe` (Name "B")

    it "should produce the correct tape from sample @N=2" $ do
      let result = runTuring . makeMachine $ testSetup 2
      currentValue result `shouldBe` Set
      currentState result `shouldBe` (Name "A")
      tapeTailLeft result `shouldBe` []
      tapeTailRight result `shouldBe` [Set]
  