module AoC.Misc (sumConsecutiveEquals, sumOppositeEquals, mapLeft, mapRight, mapFst, mapSnd) where
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
  mapLeft f (Left x) = Left $ f x
  mapLeft _ (Right r) = Right r

  mapRight :: (b -> b') -> Either a b -> Either a b'
  mapRight _ (Left l) = Left l
  mapRight f (Right x) = Right $ f x

  mapFst :: (a -> a') -> (a,b) -> (a',b)
  mapFst f (x, y) = (f x, y)

  mapSnd :: (b -> b') -> (a,b) -> (a,b')
  mapSnd g (x, y) = (x, g y)