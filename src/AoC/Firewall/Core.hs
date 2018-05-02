module AoC.Firewall.Core (
  Firewall (..), Layer (..),
  detectCollisions, scoreCollisions, scoreFirewall,
  undetectableDelay) where

data Firewall = Firewall [Layer]
  deriving (Eq, Show)

data Layer = Layer Int Int
  deriving (Eq, Show)

detectCollisions :: Firewall -> [Layer]
detectCollisions = detectCollisionsWithDelay 0

detectCollisionsWithDelay delay (Firewall fs) = filter (isAtTopAtPosTime delay) fs


isAtTopAtPosTime :: Int -> Layer -> Bool
isAtTopAtPosTime delay l@(Layer pos _) = posAtTime (pos+delay) l == 0

{-
t: -+01234---->
    0*   *
    1 * *
    2  *
    3 /
    4/ 
-}

posAtTime :: Int -> Layer -> Int
posAtTime _ (Layer _ 1) = 0
posAtTime t (Layer pos depth) = min pos' (period - pos') where
    period = (depth - 1) * 2
    pos' = t `mod` period

scoreCollisions :: [Layer] -> Int
scoreCollisions = sum . map scoreLayer

scoreLayer (Layer pos depth) = pos * depth

scoreFirewall = scoreCollisions . detectCollisions

undetectableDelay :: Firewall -> Int
undetectableDelay firewall = head . filter (null . flip detectCollisionsWithDelay firewall) $ [0..]