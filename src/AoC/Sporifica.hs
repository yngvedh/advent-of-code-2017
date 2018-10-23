module AoC.Sporifica (
  parseSporificaGrid,
  runSimulationAndCountInfected,
) where

import qualified AoC.Sporifica.Core as C
import qualified AoC.Sporifica.Parse as P

parseSporificaGrid :: String -> Either String C.Nodes
parseSporificaGrid = P.parseGrid

runSimulationAndCountInfected :: Int -> C.Nodes -> Int
runSimulationAndCountInfected n ns = C.infected $ C.runSimulation n ns