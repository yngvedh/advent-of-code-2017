module AoC.Sporifica (
  parseSporificaGrid,
  runSimulationAndCountInfected,
  runSimulation2AndCountInfected
) where

import qualified AoC.Sporifica.Core as C
import qualified AoC.Sporifica.Parse as P
import qualified AoC.Sporifica.Simulation as S

parseSporificaGrid :: String -> Either String C.Nodes
parseSporificaGrid = P.parseGrid

runSimulationAndCountInfected :: Int -> C.Nodes -> Int
runSimulationAndCountInfected n ns = S.infected $ S.runSimulation n ns

runSimulation2AndCountInfected :: Int -> C.Nodes -> Int
runSimulation2AndCountInfected n ns = S.infected $ S.runSimulation2 n ns