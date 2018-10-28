module AoC.Sporifica.Simulation (
  SimulationResult(..), SimulationState(..), 
  stepCarrier, runSimulation, runSimulation2
) where

import qualified AoC.Sporifica.Core as C

data SimulationResult = SimulationResult { infected :: Int }
  deriving (Eq, Show)

data SimulationState = SimulationState C.Carrier C.Nodes
  deriving (Eq, Show)

withCarrier :: (C.Carrier -> C.Carrier) -> SimulationState -> SimulationState
withCarrier f (SimulationState c ns) = SimulationState (f c) ns

withCurrentNode :: (C.Node -> C.Node) -> SimulationState -> SimulationState
withCurrentNode f (SimulationState c ns) = SimulationState c (C.updateNodeState f c ns)

nullResult = SimulationResult { infected = 0 }

turnLeft, turnRight, turnBack, moveForward, toggleNode, cycleNode, stepCarrier, stepCarrier2
  :: SimulationState -> SimulationState

turnLeft = withCarrier C.turnLeft
turnRight = withCarrier C.turnRight
turnBack = withCarrier C.turnBack
moveForward = withCarrier C.moveForward
toggleNode = withCurrentNode C.toggleNode
cycleNode = withCurrentNode C.cycleNode

stepCarrier s@(SimulationState c ns) = moveForward . toggleNode . turn $ s where
  turn = case C.nodeState c ns of
    C.Infected -> turnRight
    _          -> turnLeft
  
stepCarrier2 s@(SimulationState c ns) = moveForward . cycleNode . turn $ s where
  turn = case C.nodeState c ns of
    C.Clean    -> turnLeft
    C.Weakened -> id
    C.Infected -> turnRight
    C.Flagged  -> turnBack

incInfected :: SimulationResult -> SimulationResult
incInfected r = r { infected = infected r + 1 }

runSimulation, runSimulation2 :: Int -> C.Nodes -> SimulationResult
runSimulation = runSimulation' stepCarrier
runSimulation2 = runSimulation' stepCarrier2

runSimulation' :: (SimulationState -> SimulationState) -> Int -> C.Nodes -> SimulationResult
runSimulation' stepper n ns =
  getResult . head . drop n $ iterate stepCarrier' (C.initialCarrier, nullResult, ns) where
  stepCarrier' (c, r, ns) = (c', r', ns') where
    (SimulationState c' ns') = stepper (SimulationState c ns)
    r' = if C.isInfected c ns' then incInfected r else r
  getResult (_,r,_) = r
