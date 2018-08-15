module AoC.Duet (parseDuetProgram, makeSoloExecutionContext, runSoloFirstRcv, soloOutputFrequency) where

import qualified AoC.Duet.Core as C
import qualified AoC.Duet.Parse as P
import qualified AoC.Duet.Solo as S

parseDuetProgram = P.parseInstructions
makeSoloExecutionContext :: [C.Instruction] -> S.ExecutionContext S.IoReg
makeSoloExecutionContext = S.emptyExecutionContext
runSoloFirstRcv :: (Eq a, S.Channel a) => S.ExecutionContext a -> S.ExecutionContext a
runSoloFirstRcv = S.runSoloFirstRcv
soloOutputFrequency (S.ExecutionContext _ hz) = hz