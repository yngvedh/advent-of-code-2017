module AoC.Plumbing.Core (
  Program (..), Village (..),
  getPrograms, getConnected,
  getGroup, getGroups,
  removeProgram, removePrograms) where

import Data.List (nub)

data Program = Program Int
  deriving (Eq, Show)

data Village = Village [(Program, Program)]
  deriving (Eq, Show)

getPrograms :: Village -> [Program]
getPrograms (Village ps) = nub . map fst $ ps

getConnected :: Village -> Program -> [Program]
getConnected (Village ps) p = map snd . filter (\(p',_) -> p == p') $ ps

removeProgram :: Program -> Village -> Village
removeProgram p (Village as) = Village . filter (\(a,b) -> a /= p && b /= p) $  as

removePrograms :: [Program] -> Village -> Village
removePrograms ps v = foldl (flip removeProgram) v ps

getGroup :: Village -> Program -> [Program]
getGroup v@(Village as) p = nub (p:ps) where
  ps = concat . map (getGroup v') $ neighbours
  v' = removeProgram p v
  neighbours = getConnected v p

getGroups :: Village -> [[Program]]
getGroups (Village []) = []
getGroups v@(Village as) = group:(getGroups v') where
  v' = removePrograms group v
  group = getGroup v $ fst . head $ as
