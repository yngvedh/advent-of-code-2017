import Test.Hspec

import Data.Maybe (fromJust)

import AoC.Misc (sumConsecutiveEquals, sumOppositeEquals)
import AoC.SpreadSheet as SpreadSheet
import AoC.CircularMemory as CircularMemory
import AoC.PassPhrase as PassPhrase
import AoC.CPU as CPU
import AoC.BlockMemory as BlockMemory
import AoC.RecursiveTower as RT
import AoC.RegisterInstructions.Core as RI
import AoC.RegisterInstructions.Parse as RI'

import Tests.AoC.Stream
import Tests.AoC.KnotHash
import Tests.AoC.HexGrid
import Tests.AoC.Plumbing
import Tests.AoC.Firewall
import Tests.AoC.Disk
import Tests.AoC.Focus

main :: IO ()
main = hspec $ do
  describe "One.sumConsecutiveEquals" $ do
    it "case 1122" $ do
      sumConsecutiveEquals [1,1,2,2] `shouldBe` (3 :: Int)

    it "case 1111" $
      sumConsecutiveEquals [1,1,1,1] `shouldBe` (4 :: Int)

    it "case 1234" $ do
      sumConsecutiveEquals [1,2,3,4] `shouldBe` (0 :: Int)

    it "case 91212129" $ do
      sumConsecutiveEquals [9,1,2,1,2,1,2,9] `shouldBe` (9 :: Int)

  describe "One.sumOppositeEquals" $ do
    it "1212 produces 6: the list contains 4 items, and all four digits match the digit 2 items ahead." $ do
      sumOppositeEquals [1,2,1,2] `shouldBe` 6

    it "1221 produces 0, because every comparison is between a 1 and a 2." $ do
      sumOppositeEquals [1,2,2,1] `shouldBe` 0

    it "123425 produces 4, because both 2s match each other, but no other digit has a match." $ do
      sumOppositeEquals [1,2,3,4,2,5] `shouldBe` 4

    it "123123 produces 12." $ do
      sumOppositeEquals [1,2,3,1,2,3] `shouldBe` 12

    it "12131415 produces 4." $ do
      sumOppositeEquals [1,2,1,3,1,4,1,5] `shouldBe` 4

  describe "SpreadSheet.checksum" $ do
    -- The first row's largest and smallest values are 9 and 1, and their difference is 8.
    -- The second row's largest and smallest values are 7 and 3, and their difference is 4.
    -- The third row's difference is 6.
    it "case 5 1 9 5, 7 5 3, 2 4 6 8" $ do
      SpreadSheet.checksum [[5,1,9,5],[7,5,3],[2,4,6,8]] `shouldBe` 18

  describe "SpreadSheet.sumDivisible" $ do
    -- In the first row, the only SpreadSheet numbers that evenly divide are 8 and 2; the result of this division is 4.
    it "Row #1" $ do
      SpreadSheet.findDivisible [5,9,2,8] `shouldBe` 4

    -- In the second row, the SpreadSheet numbers are 9 and 3; the result is 3.
    it "Row #2" $ do
      SpreadSheet.findDivisible [9,4,7,3] `shouldBe` 3

    -- In the third row, the result is 2.    
    it "Row #3" $ do
      SpreadSheet.findDivisible [3,8,6,5] `shouldBe` 2

    it "Sum" $ do
      SpreadSheet.sumOfDivisions [[5,9,2,8],[9,4,7,3],[3,8,6,5]] `shouldBe` 9

  describe "CircularMemory.distanceToAddress" $ do
    it "case 1" $ do
      CircularMemory.distanceToAddress 1 `shouldBe` 0

    it "case 12" $ do
      CircularMemory.distanceToAddress 12 `shouldBe` 3

    it "case 23" $ do
      CircularMemory.distanceToAddress 23 `shouldBe` 2

    it "case 1024" $ do
      CircularMemory.distanceToAddress 1024 `shouldBe` 31

  describe "CircularMemory.initialValueOfAddress" $ do
    -- Square 1 starts with the value 1.
    it "case 1" $ do
      CircularMemory.initialValueOfAddress 1 `shouldBe` 1
    -- Square 2 has only one adjacent filled square (with value 1), so it also stores 1.
    it "case 2" $ do
      CircularMemory.initialValueOfAddress 2 `shouldBe` 1

    -- Square 3 has both of the above squares as neighbors and stores the sum of their values, 2.
    it "case 3" $ do
      CircularMemory.initialValueOfAddress 3 `shouldBe` 2

    -- Square 4 has all CircularMemory of the aforementioned squares as neighbors and stores the sum of their values, 4.
    it "case 4" $ do
      CircularMemory.initialValueOfAddress 4 `shouldBe` 4

    -- Square 5 only has the first and fourth squares as neighbors, so it gets the value 5.
    it "case 5" $ do
      CircularMemory.initialValueOfAddress 5 `shouldBe` 5

  describe "CircularMemory.addressOfLocation" $ do
    it "First ring" $ do
      CircularMemory.addressOfLocation (0,0) `shouldBe` 1
      CircularMemory.addressOfLocation (1,0) `shouldBe` 2
      CircularMemory.addressOfLocation (1,1) `shouldBe` 3
      CircularMemory.addressOfLocation (0,1) `shouldBe` 4
      CircularMemory.addressOfLocation (-1,1) `shouldBe` 5
      CircularMemory.addressOfLocation (-1,0) `shouldBe` 6
      CircularMemory.addressOfLocation (-1,-1) `shouldBe` 7
      CircularMemory.addressOfLocation (0,-1) `shouldBe` 8
      CircularMemory.addressOfLocation (1,-1) `shouldBe` 9

    it "Second ring, right segment" $ do
      CircularMemory.addressOfLocation (2,-1) `shouldBe` 10
      CircularMemory.addressOfLocation (2,2) `shouldBe` 13

    it "Second ring, top segment" $ do
      CircularMemory.addressOfLocation (1,2) `shouldBe` 14
      CircularMemory.addressOfLocation (-2,2) `shouldBe` 17

    it "Second ring, left segment" $ do
      CircularMemory.addressOfLocation (-2,1) `shouldBe` 18
      CircularMemory.addressOfLocation (-2,-2) `shouldBe` 21

    it "Second ring, bottom segment" $ do
      CircularMemory.addressOfLocation (-1,-2) `shouldBe` 22
      CircularMemory.addressOfLocation (2,-2) `shouldBe` 25

  describe "CircularMemory.segmentOfLocation" $ do
    it "Second ring, bottom segment" $ do
      CircularMemory.segmentOfLocation (-2, -2) `shouldBe` CircularMemory.LeftSegment
      CircularMemory.segmentOfLocation (-1, -2) `shouldBe` CircularMemory.BottomSegment
      CircularMemory.segmentOfLocation (2, -2) `shouldBe` CircularMemory.BottomSegment

  describe "PassPhrase.isValid" $ do
    it "aa bb cc dd ee is valid" $ do
      PassPhrase.isValid "aa bb cc dd ee" `shouldBe` True

    it "aa bb cc dd aa is not valid - the word aa appears more than once." $ do
      PassPhrase.isValid "aa bb cc dd aa" `shouldBe` False

    it "aa bb cc dd aaa is valid - aa and aaa count as different words." $ do
      PassPhrase.isValid "aa bb cc dd aaa" `shouldBe` True

  describe "PassPhrase.isValidNoAnagrams" $ do
    it "abcde fghij is a valid passphrase." $ do
      PassPhrase.isValidNoAnagrams "abcde fghij" `shouldBe` True

    it "abcde xyz ecdab is not valid - the letters from the third word can be rearranged to form the first word." $ do
      PassPhrase.isValidNoAnagrams "abcde xyz ecdab" `shouldBe` False

    it "a ab abc abd abf abj is a valid passphrase, because all letters need to be used when forming another word." $ do
      PassPhrase.isValidNoAnagrams "a ab abc abd abf abj" `shouldBe` True

    it "iiii oiii ooii oooi oooo is valid." $ do
      PassPhrase.isValidNoAnagrams "iiii oiii ooii oooi oooo" `shouldBe` True

    it "oiii ioii iioi iiio is not valid - any of these words can be rearranged to form any other word." $ do
      PassPhrase.isValidNoAnagrams "oiii ioii iioi iiio" `shouldBe` False

  describe "CPU.step" $ do
    it "Should step correctly with puzzle sample" $ do
      let step0 = CPU.makeJumps [] 0 [3, 0, 1, -3] -- before we have taken any steps.
      let step1 = CPU.makeJumps [] 1 [3, 0, 1, -3]  -- jump with offset 0 (that is, don't jump at all). Fortunately, the instruction is then incremented to 1.
      let step2 = CPU.makeJumps [2] 3 [0, 1, -3] -- step forward because of the instruction we just modified. The first instruction is incremented again, now to 2.
      let step3 = CPU.makeJumps [1, 0, 4, 2] (-3) [] -- jump all the way to the end; leave a 4 behind.
      let step4 = CPU.makeJumps [2] 4 [0, 1, -2] -- go back to where we just were; increment -3 to -2.
      let step5 = CPU.makeJumpsOnly [-2, 1, 0, 5, 2] -- jump 4 steps forward, escaping the maze.
      CPU.step step0 `shouldBe` step1
      CPU.step step1 `shouldBe` step2
      CPU.step step2 `shouldBe` step3
      CPU.step step3 `shouldBe` step4
      CPU.step step4 `shouldBe` step5

  describe "BlockMemory.balanceStep" $ do
    it "Should step correcly with puzzle sample" $ do
      -- The banks start with 0, 2, 7, and 0 blocks. The third bank has the most blocks, so it is chosen for redistribution.
      let step0 = BlockMemory.makeMemory [0, 2, 7, 0]
      -- Starting with the next bank (the fourth bank) and then continuing to the first bank, the second bank, and so on, the 7 blocks are spread out over the memory banks. The fourth, first, and second banks get two blocks each, and the third bank gets one back. The final result looks like this: 2 4 1 2.
      let step1 = BlockMemory.makeMemory [2, 4, 1, 2]
      -- Next, the second bank is chosen because it contains the most blocks (four). Because there are four memory banks, each gets one block. The result is: 3 1 2 3.
      let step2 = BlockMemory.makeMemory [3, 1, 2, 3]
      -- Now, there is a tie between the first and fourth memory banks, both of which have three blocks. The first bank wins the tie, and its three blocks are distributed evenly over the other three banks, leaving it with none: 0 2 3 4.
      let step3 = BlockMemory.makeMemory [0, 2, 3, 4]
      -- The fourth bank is chosen, and its four blocks are distributed such that each of the four banks receives one: 1 3 4 1.
      let step4 = BlockMemory.makeMemory [1, 3, 4, 1]
      -- The third bank is chosen, and the same thing happens: 2 4 1 2.
      BlockMemory.balanceStep step0 `shouldBe` step1
      BlockMemory.balanceStep step1 `shouldBe` step2
      BlockMemory.balanceStep step2 `shouldBe` step3
      BlockMemory.balanceStep step3 `shouldBe` step4
      BlockMemory.balanceStep step4 `shouldBe` step1

  describe "BlockMemory.balance" $ do
    it "Should terminate correctly" $ do
      (BlockMemory.balance $ BlockMemory.makeMemory [0,2,7,0]) `shouldBe` (map BlockMemory.makeMemory
        [[0, 2, 7, 0], [2, 4, 1, 2], [3, 1, 2, 3], [0, 2, 3, 4], [1, 3, 4, 1]])

  describe "RecursiveTower.parseTower" $ do
    it "Should parse root only" $ do
      let input = "root (15)\n"
      let expected = Right $ Tower "root" 15 []
      RT.parseTower input `shouldBe` expected

    it "Should parse sample correctly" $ do
      let input = "pbga (66)\nxhth (57)\nebii (61)\nhavc (66)\nktlj (57)\nfwft (72) -> ktlj, cntj, xhth\nqoyq (66)\npadx (45) -> pbga, havc, qoyq\ntknk (41) -> ugml, padx, fwft\njptl (61)\nugml (68) -> gyxo, ebii, jptl\ngyxo (61)\ncntj (57)\n"
      let subT1 = RT.Tower "ugml" 68 [RT.Tower "gyxo" 61 [], RT.Tower "ebii" 61 [], RT.Tower "jptl" 61 []]
      let subT2 = RT.Tower "padx" 45 [RT.Tower "pbga" 66 [], RT.Tower "havc" 66 [], RT.Tower "qoyq" 66 []]
      let subT3 = RT.Tower "fwft" 72 [RT.Tower "ktlj" 57 [], RT.Tower "cntj" 57 [], RT.Tower "xhth" 57 []]
      let expected = Right $ RT.Tower "tknk" 41 [subT1, subT2, subT3]
      RT.parseTower input `shouldBe` expected

  describe "RecursiveTower.parseTower" $ do
    it "Should not find unbalanced if none are" $ do
      let balanced1 = RT.Tower "balancedr" 50 [RT.Tower "eqr" 30 [], RT.Tower "samer" 30[]]
      let balanced2 = RT.Tower "balanced" 50 [RT.Tower "eq" 20 [], RT.Tower "same" 20[], RT.Tower "similar" 20 []]
      let tower = RT.Tower "root" 100 [balanced1, balanced2]
      RT.findUnbalanced tower `shouldBe` Nothing


    it "Should find unbalanced program" $ do
      let unbalanced = RT.Tower "unbalanced" 50 [RT.Tower "smaller" 20 [], RT.Tower "bigger" 30 []]
      let balanced = RT.Tower "balanced" 50 [RT.Tower "eq" 25[], RT.Tower "same" 25[]]
      let tower = RT.Tower "root" 100 [unbalanced, balanced]
      RT.findUnbalanced tower `shouldBe` (Just unbalanced)

  describe "RecursiveTower.balancingWeight" $ do
    it "Should find the correct weight for the unbalanced program" $ do
      let subT1 = RT.Tower "ugml" 68 [RT.Tower "gyxo" 61 [], RT.Tower "ebii" 61 [], RT.Tower "jptl" 61 []]
      let subT2 = RT.Tower "padx" 45 [RT.Tower "pbga" 66 [], RT.Tower "havc" 66 [], RT.Tower "qoyq" 66 []]
      let subT3 = RT.Tower "fwft" 72 [RT.Tower "ktlj" 57 [], RT.Tower "cntj" 57 [], RT.Tower "xhth" 57 []]
      let root = RT.Tower "tknk" 41 [subT1, subT2, subT3]
      (RT.balancingWeight . fromJust . RT.findUnbalanced) root `shouldBe` 60

  describe "RegisterInstructions.Registers" $ do
    it "Should return 0 initially for all registers" $ do
      let regs = RI.makeRegisters
      RI.registerValue (RI.Name "a") regs `shouldBe` 0
      RI.registerValue (RI.Name "b") regs `shouldBe` 0

    it "Should update registers properly" $ do
      let name = RI.Name "a"
      let regs = RI.makeRegisters
      let regs' = RI.updateRegister name 42 regs
      let regs'' = RI.updateRegister name (-2) regs'
      RI.registerValue name regs' `shouldBe` 42
      RI.registerValue name regs'' `shouldBe` 40

  describe "RegisterInstructions.executeX" $ do
    it "Should execute sample correctly" $ do
      let a = RI.Name "a"
      let b = RI.Name "b"
      let c = RI.Name "c"
      let is = [RI.Instruction b 5 a RI.Greater 1, -- b inc 5 if a > 1
                RI.Instruction a 1 b RI.Less 5, -- a inc 1 if b < 5
                RI.Instruction c 10 a RI.GreaterEq 1, -- c dec -10 if a >= 1
                RI.Instruction c (-20) c RI.Equal 10] -- c inc -20 if c == 10
      let expected = Registers [(c,-10), (a, 1)]
      let result = RI.executeInstructions makeRegisters is
      result `shouldBe` expected

  describe "RegisterInstructions.parseInstructions" $ do
    it "Should parse sample correctly" $ do
      let input = "b inc 5 if a > 1\na inc 1 if b < 5\nc dec -10 if a >= 1\nc inc -20 if c == 10\n"
      let a = RI.Name "a"
      let b = RI.Name "b"
      let c = RI.Name "c"
      let expected = Right [RI.Instruction b 5 a RI.Greater 1, -- b inc 5 if a > 1
                RI.Instruction a 1 b RI.Less 5, -- a inc 1 if b < 5
                RI.Instruction c 10 a RI.GreaterEq 1, -- c dec -10 if a >= 1
                RI.Instruction c (-20) c RI.Equal 10] -- c inc -20 if c == 10
      let result = RI'.parseInstructions input
      result `shouldBe` expected

    it "Should parse 'lyr dec -970 if gm != 0\n' correctly" $ do
      let input = "lyr dec -970 if gm != 0\n"
      let expected = Right [RI.Instruction (RI.Name "lyr") 970 (RI.Name "gm") RI.Inequal 0]
      RI'.parseInstructions input `shouldBe` expected  

  describe "RegisterInstruction.executeInstructionsAndTraceLargestValue" $ do
    it "Should trace sample correctly" $ do
      let a = RI.Name "a"
      let b = RI.Name "b"
      let c = RI.Name "c"
      let is = [RI.Instruction b 5 a RI.Greater 1, -- b inc 5 if a > 1
                RI.Instruction a 1 b RI.Less 5, -- a inc 1 if b < 5
                RI.Instruction c 10 a RI.GreaterEq 1, -- c dec -10 if a >= 1
                RI.Instruction c (-20) c RI.Equal 10] -- c inc -20 if c == 10
      let (regs, largest) = RI.executeInstructionsAndTraceLargestValue makeRegisters is
      largest `shouldBe` 10

  describeStreamParse
  describeStreamScore
  describeStreamCountGarbage
  describeKnotHash
  describeHexGrid
  describePlumbing
  describeFirewall
  describeDisk
  describeFocus
