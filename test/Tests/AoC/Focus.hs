module Tests.AoC.Focus (describeFocus) where

import qualified AoC.Focus.List as L
import qualified AoC.Focus.ListMatrix as M

import Test.Hspec

describeFocus = describe "AoC.Focus" $ do
  describe "ListFocus" $ do
    it "should initially focus on the first item in the list" $ do
      let focus = L.makeFocus [1,2..]
      L.get focus`shouldBe` 1

    it "should move right" $ do
      let focus = L.makeFocus [1,2,3]
      let focus' = L.moveRight focus
      L.get focus' `shouldBe` 2
      let focus'' = L.moveRight focus'
      L.get focus'' `shouldBe` 3

    it "should not move left when leftmost" $ do
      let focus = L.makeFocus [1,2]
      L.moveLeft focus `shouldBe` focus

    it "should not move right when rightmost" $ do
      let focus = L.makeFocus [1]
      L.moveRight focus `shouldBe` focus

    it "should set the head element" $ do
      let focus = L.makeFocus [1,2,3]
      let focus' = L.set 4 focus
      L.unfocus focus' `shouldBe` [4,2,3]

    it "should set a non-head element" $ do
      let focus = L.makeFocus [1,2,3,4]
      let focus' = L.set 5 . L.moveRight . L.moveRight $ focus
      L.unfocus focus' `shouldBe` [1,2,5,4]

    it "should update an element" $ do
      let focus = L.makeFocus [1,2,3,4]
      let focus' = L.update ((+) 8) . L.moveRight $ focus
      L.unfocus focus' `shouldBe` [1,10,3,4]

  describe "ListMatrixFocus" $ do
    it "should initially focus on the first row and column" $ do
      let focus = M.makeFocus [[1,2],[3,4]]
      M.get focus `shouldBe` 1

    it "should not move left when leftmost" $ do
      let f = M.makeFocus [[1]]
      M.moveLeft f `shouldBe` f

    it "should not move right when rightmost" $ do
      let f = M.makeFocus [[1]]
      M.moveRight f `shouldBe` f

    it "should not move up when topmost" $ do
      let f = M.makeFocus [[1]]
      M.moveUp f `shouldBe` f

    it "should not move down when bottommost" $ do
      let f = M.makeFocus [[1]]
      M.moveDown f `shouldBe` f

    it "should move right correctly" $ do
      let f = M.moveRight . M.makeFocus $ [[1, 2], [3, 4]]
      M.get f `shouldBe` 2

    it "should move down correctly" $ do
      let f = M.moveDown . M.makeFocus $ [[1, 2], [3, 4]]
      M.get f `shouldBe` 3

    it "should move up correctly" $ do
      let f = M.moveUp . M.moveRight . M.moveDown . M.makeFocus $ [[1, 2], [3, 4]]
      M.get f `shouldBe` 2

    it "should move left correctly" $ do
      let f = M.moveLeft . M.moveDown . M.moveRight . M.makeFocus $ [[1, 2], [3, 4]]
      M.get f `shouldBe` 3
