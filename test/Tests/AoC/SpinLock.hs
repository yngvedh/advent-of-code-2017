module Tests.AoC.SpinLock (describeSpinLock) where

import AoC.SpinLock.Core

import Test.Hspec

describeSpinLock = describe "AoC.SpinLock" $ do
  describe "makeSpinLock" $ do
    it "should make a lock in the first position" $ do
      let s = makeSpinLock 0 [1,2,3]
      currentValue s `shouldBe` 1

    it "should make a lock in the middle position" $ do
      let s = makeSpinLock 1 [1,2,3]
      currentValue s `shouldBe` 2

    it "should make a lock in the last position" $ do
      let s = makeSpinLock 2 [1,2,3]
      currentValue s `shouldBe` 3


  describe "spinRight" $ do
    it "should move from first position to the next" $ do
      let s = makeSpinLock 0 [1,2,3]
      let s' = spinRight 1 s
      currentValue s' `shouldBe` 2

    it "should move from middle position to the last" $ do
      let s = makeSpinLock 1 [1,2,3]
      let s' = spinRight 1 s
      currentValue s' `shouldBe` 3

    it "should move from last position to the first" $ do
      let s = makeSpinLock 2 [1,2,3]
      let s' = spinRight 1 s
      currentValue s' `shouldBe` 1

    it "should cycle back to initial position when spin = size" $ do
      let s = makeSpinLock 1 [1,2,3,4,5,6,7,8,9,0]
      let s' = spinRight 10 s
      currentValue s' `shouldBe` 2

    it "should always be positioned at the only element" $ do
      let s = makeSpinLock 0 [1]
      let s' = spinRight 5 s
      currentValue s' `shouldBe` 1

  describe "insert" $ do
    it "should insert after the only element" $ do
      let s = makeSpinLock 0 [1]
      let s' = insert 2 s
      currentValue s' `shouldBe` 2

    it "should insert after an element in the middle" $ do
      let s = makeSpinLock 3 [1..5]
      let s' = insert 6 s
      (currentValue . spinRight 1) s' `shouldBe` 5

    it "should insert element after the last element" $ do
      let s = makeSpinLock 4 [1..5]
      let s' = insert 6 s
      (currentValue . spinRight 1) s' `shouldBe` 1

  describe "spin and spinN" $ do
    let s0 = makeSpinLock 0 [0]
    let s1 = makeSpinLock 1 [0, 1]
    let s2 = makeSpinLock 1 [0, 2, 1]
    let s3 = makeSpinLock 2 [0, 2, 3, 1]
    let s4 = makeSpinLock 2 [0, 2, 4, 3, 1]
    let s5 = makeSpinLock 1 [0, 5, 2, 4, 3, 1]
    let s6 = makeSpinLock 5 [0, 5, 2, 4, 3, 6, 1]
    let s7 = makeSpinLock 2 [0, 5, 7, 2, 4, 3, 6, 1]
    let s8 = makeSpinLock 6 [0, 5, 7, 2, 4, 3, 8, 6, 1]
    let s9 = makeSpinLock 1 [0, 9, 5, 7, 2, 4, 3, 8, 6, 1]

    it "should match sample after every step" $ do
      spin 3 1 s0 `shouldBe` s1
      spin 3 2 s1 `shouldBe` s2
      spin 3 3 s2 `shouldBe` s3
      spin 3 4 s3 `shouldBe` s4
      spin 3 5 s4 `shouldBe` s5
      spin 3 6 s5 `shouldBe` s6
      spin 3 7 s6 `shouldBe` s7
      spin 3 8 s7 `shouldBe` s8
      spin 3 9 s8 `shouldBe` s9

    it "should match sample after 9 steps" $ do
      spinN 9 3 1 s0 `shouldBe` s9

  describe "simulateValueAtPos1" $ do
    it "should produce the same values at pos 1 as the sample" $ do
      map (simulateValueAtPos1 3) [1..9] `shouldBe` [1,2,2,2,5,5,5,5,9]



