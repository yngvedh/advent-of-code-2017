module AoC.Focus.ListMatrix (
  ListMatrixFocus,
  makeFocus, unfocus,
  moveDown, moveUp, moveRight, moveLeft,
  moveLeftMost, moveTopMost,
  isLeftMost, isTopMost, isBottomMost, isRightMost,
  focusElement,
  get, set, update) where

import qualified AoC.Focus.List as L

data ListMatrixFocus a = ListMatrixFocus (L.ListFocus (L.ListFocus a))
  deriving (Eq, Show)

matrix (ListMatrixFocus a) = a

makeFocus :: [[a]] -> ListMatrixFocus a
makeFocus = ListMatrixFocus . L.makeFocus . map L.makeFocus

unfocus :: ListMatrixFocus a -> [[a]]
unfocus = map L.unfocus . L.unfocus . matrix

moveUp = withList L.moveLeft
moveDown = withList L.moveRight

moveLeft = withList $ fmap L.moveLeft
moveRight = withList $ fmap L.moveRight

moveLeftMost = withList $ fmap L.moveLeftMost
moveTopMost :: ListMatrixFocus a -> ListMatrixFocus a
moveTopMost = withList $ L.moveLeftMost

isLeftMost = L.isLeftMost . L.get . matrix
isRightMost = L.isRightMost . L.get . matrix
isTopMost = L.isLeftMost . matrix
isBottomMost = L.isRightMost . matrix

focusElement :: (a -> Bool) -> ListMatrixFocus a -> Maybe (ListMatrixFocus a)
focusElement pred = focusElement' pred . moveTopMost . moveLeftMost

focusElement' :: (a -> Bool) -> ListMatrixFocus a -> Maybe (ListMatrixFocus a)
focusElement' pred f =
  if pred (get f) then Just f
  else if isBottomMost f && isRightMost f then Nothing
  else if isBottomMost f then focusElement' pred . moveRight . moveTopMost $ f
  else focusElement' pred . moveDown $ f

get :: ListMatrixFocus a -> a
get = L.get . L.get . matrix

set :: a -> ListMatrixFocus a -> ListMatrixFocus a
set v = withList $ L.update (L.set v)

update :: (a -> a) -> ListMatrixFocus a -> ListMatrixFocus a
update f = withList $ L.update (L.update f)

withList f = ListMatrixFocus . f . matrix
