module AoC.Sporifica.Core (
  Nodes (..), Node (..), Carrier (..),
  Pos (..), Vec (..),
  makeGrid, makeGridFromPositions,
  initialCarrier,
  turnLeft, turnRight, turnBack, moveForward,
  toggleNode, cycleNode,
  nodeState, setNodeState, updateNodeState, isInfected) where

import qualified Data.Map.Strict as S

data Nodes = Nodes (S.Map Pos Node)
  deriving (Show, Eq)

data Node = Clean | Infected | Weakened | Flagged
  deriving (Show, Eq)

data Pos = Pos { posX :: Int, posY :: Int }
  deriving (Eq, Show, Ord)
data Vec = Vec { vecX :: Int, vecY :: Int }
  deriving (Eq, Show)

makeGrid :: [[Node]] -> Nodes
makeGrid cs = Nodes . S.fromList $ zip infectedPositions (repeat Infected) where
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

makeGridFromPositions :: [Pos] -> Nodes
makeGridFromPositions = Nodes . S.fromList . flip zip (repeat Infected)

data Carrier = Carrier {
  carrierPosition :: Pos,
  carrierHeading :: Vec
} deriving (Eq,Show)

initialCarrier = Carrier {
  carrierPosition = Pos 0 0,
  carrierHeading = Vec 0 1
}

turnLeft, turnRight, turnBack, moveForward :: Carrier -> Carrier
turnLeft c = c { carrierHeading = Vec (-(vecY h)) (vecX h) } where
  h = carrierHeading c

turnRight c = c { carrierHeading = Vec (vecY h) (-(vecX h)) } where
  h = carrierHeading c

turnBack c = c { carrierHeading = Vec (-(vecX h)) (-(vecY h)) } where
  h = carrierHeading c

moveForward c = c { carrierPosition = Pos (x+x') (y+y') } where
  (Pos x y) = carrierPosition c
  (Vec x' y') = carrierHeading c

nodeState :: Carrier -> Nodes -> Node
nodeState c (Nodes m) = case S.lookup (carrierPosition c) m of
  Just n -> n
  _ -> Clean

setNodeState :: Carrier -> Nodes -> Node -> Nodes
setNodeState c (Nodes m) Clean = Nodes $ S.delete (carrierPosition c) m
setNodeState c (Nodes m) n = Nodes $ S.insert (carrierPosition c) n m

updateNodeState :: (Node -> Node) -> Carrier -> Nodes -> Nodes
updateNodeState f c ns = setNodeState c ns (f s) where
  s = nodeState c ns

isInfected :: Carrier -> Nodes -> Bool
isInfected c ns = case nodeState c ns of
  Infected -> True
  _        -> False

toggleNode Infected = Clean
toggleNode Clean = Infected

cycleNode Clean = Weakened
cycleNode Weakened = Infected
cycleNode Infected = Flagged
cycleNode Flagged = Clean
