module AoC.Duet (parseDuetProgram, makeDuetExecutionContext, executeFirstRcv, duetOutputFrequency) where

import qualified AoC.Duet.Core as C
import qualified AoC.Duet.Parse as P

parseDuetProgram = P.parseInstructions
makeDuetExecutionContext = C.emptyExecutionContext
executeFirstRcv = C.executeFirstRcv
duetOutputFrequency (C.ExecutionContext (C.Cpu _ hz) _) = hz