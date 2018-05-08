module AoC.SpinLock.Core (
  SpinLock(..),
  makeSpinLock, emptySpinLock,
  spinRight, insert ,
  currentValue,
  spin, spinN,
  simulateValueAtPos1) where

import AoC.Focus.List

data SpinLock = SpinLock (ListFocus Int)
  deriving (Show, Eq)

emptySpinLock = makeSpinLock 0 [0]

makeSpinLock :: Int -> [Int] -> SpinLock
makeSpinLock n is = SpinLock $ makeFocusAt n is

spinRight :: Int -> SpinLock -> SpinLock
spinRight n s = if n == 0 then s else spinRight (n-1) (moveNext s)

moveNext :: SpinLock -> SpinLock
moveNext = withFocus (\f -> if isRightMost f then moveLeftMost f else moveRight f)

insert :: Int -> SpinLock -> SpinLock
insert i = withFocus $ moveRight . insertRightOf i

currentValue :: SpinLock -> Int
currentValue (SpinLock f) = get f

withFocus :: (ListFocus Int -> ListFocus Int) -> SpinLock -> SpinLock
withFocus g (SpinLock f) = SpinLock . g $ f

spin :: Int -> Int -> SpinLock -> SpinLock
spin skip value s = insert value . spinRight skip $ s

spinN :: Int -> Int -> Int -> SpinLock -> SpinLock
spinN 0 _ _ s = s
spinN n skip value s = spinN (n-1) skip (value + 1) $ spin skip value s

simulateValueAtPos1 :: Int -> Int -> Int
simulateValueAtPos1 skip n = sim 1 0 0 where
  sim size pos res =
    if size == (n+1)
    then res
    else sim (size+1) pos' res'  where
      pos' = ((pos + skip) `mod` size) + 1
      res' = if pos' == 1 then size else res
