module AoC.Duet (parseDuetProgram, makeSoloExecutionContext, runSoloFirstRcv, soloOutputFrequency) where

import qualified AoC.Duet.Core as C
import qualified AoC.Duet.Parse as P
import qualified AoC.Duet.Solo as S

parseDuetProgram = P.parseInstructions
makeSoloExecutionContext = S.emptyExecutionContext
runSoloFirstRcv = S.runSoloFirstRcv
soloOutputFrequency (S.ExecutionContext (C.Cpu _ hz) _) = hz