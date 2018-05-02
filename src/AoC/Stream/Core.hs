module AoC.Stream.Core (Group(..), score, countGarbage) where

data Group = Group [Group] | Garbage String
  deriving (Show, Eq)

score :: Group -> Int
score = score' 1 where
  score' lvl (Group gs) = lvl + subsums where
    subsums = sum . map score'' $ gs where
    score'' = score' $ lvl+1
  score' _ (Garbage _) = 0

countGarbage :: Group -> Int
countGarbage (Garbage g) = length g
countGarbage (Group gs) = sum . map countGarbage $ gs