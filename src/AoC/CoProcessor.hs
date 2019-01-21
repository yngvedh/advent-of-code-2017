module AoC.CoProcessor (
  parseCoProcessorProgram,
  makeCoProcessorExecutionContext,
  runCoProcessor,
  countMuls
) where

import qualified AoC.Duet.Core as C
import qualified AoC.Duet.Solo as S
import qualified AoC.Duet.Parse as P
import qualified AoC.Duet.Channel as Ch

import Debug.Trace

parseCoProcessorProgram = P.parseInstructions

makeCoProcessorExecutionContext :: [C.Instruction] -> S.ExecutionContext S.IoReg
makeCoProcessorExecutionContext = S.emptyExecutionContext

runCoProcessor :: (Ch.Channel a, Eq a, Show a) => S.ExecutionContext a -> Either S.ExecutionLog (S.ExecutionContext a)
runCoProcessor = S.runSolo

countMuls :: Show a => Either S.ExecutionLog (S.ExecutionContext a) -> Int
countMuls res = S.numMuls . S.getEitherLog $ traceShow res res
