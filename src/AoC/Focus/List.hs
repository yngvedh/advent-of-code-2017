module AoC.Focus.List (
  ListFocus, makeFocus, makeFocusAt,
  moveRight, moveLeft, moveLeftMost,
  isLeftMost, isRightMost,
  set, get, update,
  focusElement,
  unfocus,
  insertRightOf) where

data ListFocus a = ListFocus [a] [a]
  deriving (Show, Eq)

makeFocus :: [a] -> ListFocus a
makeFocus [] = error "Attempting to construct a ListFocus with an empty list"
makeFocus as = ListFocus [] as

makeFocusAt :: Int -> [a] -> ListFocus a
makeFocusAt _ [] = error "Attempting to construct a ListFocus with an empty list" 
makeFocusAt n as = ListFocus (reverse . take n $ as) (drop n as)

unfocus (ListFocus pre post) = (reverse pre) ++ post

moveRight (ListFocus pre (p:post@(_:_))) = ListFocus (p:pre) post
moveRight lf = lf

moveLeft (ListFocus (p:pre) post) = ListFocus pre (p:post)
moveLeft lf = lf

moveLeftMost = makeFocus . unfocus

isLeftMost (ListFocus [] _) = True
isLeftMost _ = False

isRightMost (ListFocus _ [p]) = True
isRightMost _ = False

insertRightOf :: a -> ListFocus a -> ListFocus a
insertRightOf a (ListFocus pre (p:post)) = ListFocus pre (p:a:post)

set :: a -> ListFocus a -> ListFocus a
set f' (ListFocus pre (p:post)) = (ListFocus pre (f':post))

get :: ListFocus a -> a
get (ListFocus _ (p:post)) = p

update :: (a -> a) -> ListFocus a -> ListFocus a
update f (ListFocus pre (p:post)) = ListFocus pre ((f p):post)

focusElement :: (a -> Bool) -> ListFocus a -> Maybe (ListFocus a)
focusElement pred f =
  case findRight f of
    Just f' -> Just f'
    Nothing -> findLeft f
  where
    findLeft f' =
      if pred $ get f' then Just f'
      else if isLeftMost f' then Nothing
      else findLeft $ moveLeft f'
    findRight f' =
      if pred $ get f' then Just f'
      else if isRightMost f' then Nothing
      else findRight $ moveRight f'

instance Functor ListFocus where
  fmap f (ListFocus pre post) = (ListFocus (map f pre) (map f post))