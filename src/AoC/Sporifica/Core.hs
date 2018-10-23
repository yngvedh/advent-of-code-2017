module AoC.Sporifica.Core (
  Nodes (..), Node (..), Carrier (..),
  Pos (..), Vec (..),
  SimulationResult
 (..),
  makeGrid, makeGridFromPositions,
  stepCarrier, runSimulation) where

import qualified Data.Set as S

data Nodes = Nodes (S.Set Pos)
  deriving (Show, Eq)

data Node = Clean | Infected
  deriving (Show, Eq)

data Pos = Pos { posX :: Int, posY :: Int }
  deriving (Eq, Show, Ord)
data Vec = Vec { vecX :: Int, vecY :: Int }
  deriving (Eq, Show)

data SimulationResult = SimulationResult { infected :: Int }
    deriving (Eq, Show)

nullResult = SimulationResult { infected = 0 }

makeGrid :: [[Node]] -> Nodes
makeGrid cs = Nodes . S.fromList $ infectedPositions where
  infectedPositions = map fst . filter ((==) Infected . snd) $ assocList
  assocList :: [(Pos,Node)]
  assocList = concat . injectCoordinates $ cs
  w = length . head $ cs
  h = length cs
  w' = (w-1) `div` 2
  h' = (h-1) `div` 2
  injectCoordinates = zipWith f (reverse [(-h')..h']) . map (zip [(-w')..w'])
  f :: Int -> [(Int, Node)] -> [(Pos,Node)]
  f y = map (\(x, c) -> (Pos x y, c))

makeGridFromPositions = Nodes . S.fromList

data Carrier = Carrier {
  carrierPosition :: Pos,
  carrierHeading :: Vec
} deriving (Eq,Show)

initialCarrier = Carrier {
  carrierPosition = Pos 0 0,
  carrierHeading = Vec 0 1
}

turnLeft, turnRight, moveForward :: Carrier -> Carrier
turnLeft c = c { carrierHeading = Vec (-(vecY h)) (vecX h) } where
  h = carrierHeading c

turnRight c = c { carrierHeading = Vec (vecY h) (-(vecX h)) } where
  h = carrierHeading c

moveForward c = c { carrierPosition = Pos (x+x') (y+y') } where
  (Pos x y) = carrierPosition c
  (Vec x' y') = carrierHeading c

isInfected :: Carrier -> Nodes -> Bool
isInfected c (Nodes m) = (carrierPosition c) `elem` m

toggleInfected :: Carrier -> Nodes -> Nodes
toggleInfected c ns@(Nodes m) = 
  if isInfected c ns
    then Nodes . S.delete pos $ m
    else Nodes . S.insert pos $ m
  where
    pos = carrierPosition c

stepCarrier :: (Carrier, Nodes) -> (Carrier, Nodes)
stepCarrier (c, ns) =
  if isInfected c ns
    then move' . toggle' . turnRight' $ (c,ns)
    else move' . toggle' . turnLeft' $ (c,ns)
  where
    turnLeft' (c, ns) = (turnLeft c, ns)
    turnRight' (c, ns) = (turnRight c, ns)
    move' (c, ns) = (moveForward c, ns)
    toggle' (c, ns) = (c, toggleInfected c ns)

incInfected :: SimulationResult -> SimulationResult
incInfected r = r { infected = infected r + 1 }

runSimulation :: Int -> Nodes -> SimulationResult
runSimulation n ns =
  getResult . head . drop n $ iterate stepCarrier' (initialCarrier, nullResult, ns) where
  stepCarrier' (c, r, ns) = (c', r', ns') where
    (c', ns') = stepCarrier (c, ns)
    r' = if isInfected c ns' then incInfected r else r
  getResult (_,r,_) = r
