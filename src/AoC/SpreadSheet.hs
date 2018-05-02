module AoC.SpreadSheet (checksum, findDivisible, sumOfDivisions) where

  import Data.List (find)

  checksum :: [[Int]] -> Int
  checksum rs = sum $ map minmax rs where
    minmax xs = (foldr1 max xs) - (foldr1 min xs)

  findDivisible :: [Int] -> Int
  findDivisible row = let
    divisible = find divides pairs
    pairs = [(a,b) | a <- row, b <- row, a > b]
    divides (a,b) = (a `mod` b) == 0 in
      case divisible of
        (Just (a,b)) -> a `div` b
        Nothing -> 0

  sumOfDivisions :: [[Int]] -> Int
  sumOfDivisions sheet = sum $ map findDivisible sheet
  