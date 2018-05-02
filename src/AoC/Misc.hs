module AoC.Misc (sumConsecutiveEquals, sumOppositeEquals, mapLeft) where
  sumConsecutiveEquals :: [Int] -> Int
  sumConsecutiveEquals [] = 0
  sumConsecutiveEquals (x:xs) = sce x (x:xs) where
    sce h [] = 0
    sce h (x:[]) = if x == h then x else 0
    sce h (x:x':xs) = if x == x' then x + rest else rest where
      rest = sce h (x':xs)

  sumOppositeEquals :: [Int] -> Int
  sumOppositeEquals x = sumEquals $ zip x x' where
    x' = take l $ drop (l `div` 2) $ cycle x
    l = length x
    sumEquals [] = 0
    sumEquals ((x,x'):xs) = if x == x' then x + rest else rest where
      rest = sumEquals xs

  mapLeft :: (a -> a') -> Either a b -> Either a' b
  mapLeft f (Left x) = Left . f $ x
  mapLeft _ (Right r) = Right r