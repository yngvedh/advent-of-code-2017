module AoC.ParticleSwarm.Core (
  Particle (..), Particle3d(..), Show, Eq,
  findClosestLongTerm, findRemainingAfterCollisions,
  computeCollisions, computeCollisionTime, computeQuadratic, computeLinear) where

import Data.List (sortBy, sort, nub, groupBy, elemIndex, union, intersect)
import Data.Maybe (fromJust, catMaybes, isJust)

import AoC.Misc (mapFst)


data Particle a = Particle { pos :: a, vel :: a, acc :: a }

instance (Show a) => Show (Particle a) where
  show (Particle p v a) = "Particle { pos = " ++ (show p) ++", vel = " ++ show v ++ ", acc = " ++ show a ++" }"

instance (Eq a) => Eq (Particle a) where
  (==) (Particle p v a) (Particle p' v' a') = p == p' || v == v' || a == a'

instance Functor Particle where
  fmap f p = Particle (f . pos $ p) (f . vel $ p) (f . acc $ p)

type Particle3d = Particle (Int, Int, Int)
type Particle1d = Particle Int

findClosestLongTerm :: [Particle3d] -> Int
findClosestLongTerm ps = i where
  i = fromJust . elemIndex closest $ ps'
  closest = minimum ps'
  ps' = map dist ps
  dist p = (make1d . pos $ p, make1d . vel $ p, make1d . acc $ p)

findRemainingAfterCollisions :: [Particle3d] -> Int
findRemainingAfterCollisions [] = 0
findRemainingAfterCollisions ps = findRem [1..(length ps)] $ computeCollisions ps where
  findRem :: [Int] -> [(Int,(Int,Int))] -> Int
  findRem ns [] = length ns
  findRem ns cs =
    if null collided then error "cs is not empty, but collided is"
    else findRem ns' cs'
    where
      t0 = time . head $ cs
      collided = filter ((==) t0 . fst) cs
      collidedIds = concatMap names collided
      ns' = filter (not . flip elem collidedIds) ns
      cs' = filter (not . didCollide) cs
      didCollide = any (`elem` collidedIds) . names
      time = fst
      names (_,(a,b)) = [a,b]

combinations :: [a] -> [(a,a)]
combinations (a:as) = combsWithA ++ (combinations as) where
  combsWithA = zip (repeat a) as
combinations [] = []

nameParticles :: [Particle a] -> [(Int, Particle a)]
nameParticles = zip [1..]

computeCollisions :: [Particle3d] -> [(Int,(Int,Int))]
computeCollisions ps = ts' where
  ns = nameParticles ps
  cs = combinations ns
  ts = map (\((n,p),(n',p')) -> (computeCollisionTime p p', (n,n'))) cs
  ts' = map (mapFst fromJust) . filter (isJust . fst) $ ts

make1d (x,y,z) = x+y+z

xParticle, yParticle, zParticle :: Particle3d -> Particle1d
xParticle = fmap xCoord
yParticle = fmap yCoord
zParticle = fmap zCoord

xCoord (x,_,_) = x
yCoord (_,y,_) = y
zCoord (_,_,z) = z

computeCollisionTime :: Particle3d -> Particle3d -> Maybe Int
computeCollisionTime p p' = t0 where
  t0 = earliestCollision t
  t = foldl1 mergeCollisions [tz,tx,ty]
  tx = computeCollisionTimes1d x x'
  ty = computeCollisionTimes1d y y'
  tz = computeCollisionTimes1d z z'
  x = xParticle p
  y = yParticle p
  z = zParticle p
  x' = xParticle p'
  y' = yParticle p'
  z' = zParticle p'
  count n = length . filter ((==) n)

data CollisionTime = Never | Discrete [Int] | Always
  deriving (Show, Eq)

withDiscrete :: ([Int] -> [Int]) -> CollisionTime -> CollisionTime
withDiscrete f (Discrete ts) = if null ts' then Never else Discrete ts' where
  ts' = f ts
withDiscrete _ ct = ct

mergeCollisions :: CollisionTime -> CollisionTime -> CollisionTime
mergeCollisions Never _ = Never
mergeCollisions _ Never = Never
mergeCollisions Always b = b
mergeCollisions a Always = a
mergeCollisions (Discrete as) (Discrete bs) = Discrete $ as `intersect` bs

earliestCollision :: CollisionTime -> Maybe Int
earliestCollision Never = Nothing
earliestCollision (Discrete ts) = if null ts' then Nothing else Just $ minimum ts' where
  ts' = filter ((<) 0) $ ts
earliestCollision Always = Just 0

data Quadratic = Quadratic Int Int Int
  deriving (Show)

computeCollisionTimes1d :: Particle1d -> Particle1d -> CollisionTime
computeCollisionTimes1d p p' = 
  if a == 0 && b == 0 then
    if c == 0 then Always
    else Never
  else if null solutions
    then Never
    else Discrete solutions
  where
    f x = a*x*x + b*x + c
    q@(Quadratic a b c) = toQuadratic $ delta p p'
    ts = computeQuadratic q
    approximateSolutions = nub . sort . concat . map (\t -> [floor t - 1, floor t, ceiling t, ceiling t + 1]) $ ts
    solutions = filter ((==) 0 . evaluate q) $ approximateSolutions

delta :: Particle1d -> Particle1d -> Particle1d
delta (Particle p v a) (Particle p' v' a') = Particle (p' - p) (v' - v) (a' - a)

toQuadratic :: Particle1d -> Quadratic
toQuadratic (Particle p v a) = Quadratic (2*p) (2*v+a) a

evaluate (Quadratic p v a) x = p + x*v + x*x*a
    
computeQuadratic :: Quadratic -> [Double]
computeQuadratic (Quadratic xi0 xi1 xi2) =
  if xi2 == 0 then computeLinear xi0 xi1
  else if det < 0 then []
  else [t1, t2]
  where
    x0 = fromIntegral xi0 :: Double
    x1 = fromIntegral xi1 :: Double
    x2 = fromIntegral xi2 :: Double
    det = x1*x1 - 4*x2*x0
    det' = sqrt det
    t1 = ((-x1) + det') / (2*x2)
    t2 = ((-x1) - det') / (2*x2)

computeLinear :: Int -> Int -> [Double]
computeLinear x0 x1 =
  if x1 == 0 then
    if x0 == 0 then [0]
    else []
  else [(-(fromIntegral x0)) / (fromIntegral x1)]
