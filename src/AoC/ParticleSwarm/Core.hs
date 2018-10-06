module AoC.ParticleSwarm.Core (
  Particle (..), Particle3d(..), Show, Eq,
  findClosestLongTerm, findRemainingAfterCollisions) where

import Data.List (findIndex)
import Data.Maybe (fromJust)

data Particle a = Particle { pos :: a, vel :: a, acc :: a }

instance (Show a) => Show (Particle a) where
  show (Particle p v a) = "Particle (Pos " ++ (show p) ++") (Vel " ++ show v ++ ") (Acc = " ++ show a ++")"

instance (Eq a) => Eq (Particle a) where
  (==) (Particle p v a) (Particle p' v' a') = p == p' || v == v' || a == a'

instance Functor Particle where
  fmap f p = Particle (f . pos $ p) (f . vel $ p) (f . acc $ p) 

type Particle3d = Particle (Int, Int, Int)
type Particle1d = Particle Int

findClosestLongTerm :: [Particle3d] -> Int
findClosestLongTerm ps = i where
  i = fromJust . findIndex ((==) closest) $ ps'
  closest = minimum ps'
  ps' = map dist ps
  dist p = (make1d . pos $ p, make1d . vel $ p, make1d . acc $ p)
  
findRemainingAfterCollisions :: [Particle3d] -> Int
findRemainingAfterCollisions ps = undefined where
  ls = map make1dParticle ps

make1dParticle :: Particle3d -> Particle1d
make1dParticle = fmap make1d

make1d (x,y,z) = (abs x)+(abs y)+(abs z)

-- given particles a,b
-- compute delta particle d
-- compute the manhattan distance of all axes so that we can solve it in 1 dim.
-- does d hit exactly (0,0,0 at ny point?)
--  transform to 2nd degree equation and solve for all
