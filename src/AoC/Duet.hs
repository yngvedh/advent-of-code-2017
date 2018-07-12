module AoC.Duet (parseDuetProgram, makeDuetExecutionContext, runSoloFirstRcv, duetOutputFrequency) where

import qualified AoC.Duet.Core as C
import qualified AoC.Duet.Parse as P

parseDuetProgram = P.parseInstructions
makeDuetExecutionContext = C.emptyExecutionContext
runSoloFirstRcv = C.runSoloFirstRcv
duetOutputFrequency (C.ExecutionContext (C.Cpu _ hz) _) = hz