module AoC.ParticleSwarm.Core (
  Particle (..), Pos(..), Vel(..), Acc(..),
  findClosestLongTerm, findRemainingAfterCollisions) where

import Data.List (findIndex)
import Data.Maybe (fromJust)

data Pos = Pos Int Int Int
  deriving (Show, Eq)

data Vel = Vel Int Int Int
  deriving (Show, Eq)

data Acc = Acc Int Int Int
  deriving (Show, Eq)
  
data Particle = Particle Pos Vel Acc
  deriving (Show, Eq)

findClosestLongTerm :: [Particle] -> Int
findClosestLongTerm ps = i where
  i = fromJust . findIndex ((==) closest) $ ps'
  closest = minimum ps'
  ps' = map dist ps
  dist (Particle (Pos x y z) (Vel x' y' z') (Acc x'' y'' z'')) =
    ((abs x'')+(abs y'')+(abs z''),
     (abs x' )+(abs y' )+(abs z' ),
     (abs x  )+(abs y  )+(abs z  ))
  
findRemainingAfterCollisions :: [Particle] -> Int
findRemainingAfterCollisions = undefined
