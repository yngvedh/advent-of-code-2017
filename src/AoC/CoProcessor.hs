module AoC.CoProcessor (
  parseCoProcessorProgram,
  makeCoProcessorExecutionContext,
  runCoProcessor,
  countMuls,
  runCountNonPrimesOptimized,
  runCountMulsOptimized
) where

import qualified AoC.Duet.Core as C
import qualified AoC.Duet.Solo as S
import qualified AoC.Duet.Parse as P
import qualified AoC.Duet.Channel as Ch

import Debug.Trace

parseCoProcessorProgram = P.parseInstructions

makeCoProcessorExecutionContext :: [C.Instruction] -> S.ExecutionContext S.IoReg
makeCoProcessorExecutionContext = S.emptyExecutionContext

runCoProcessor :: (Ch.Channel a, Eq a, Show a) => S.ExecutionContext a -> S.ExecutionContext a
runCoProcessor = S.runSolo

countMuls :: Show a => S.ExecutionContext a -> Int
countMuls res = S.numMuls . S.executionLog $ res

runCountNonPrimesOptimized, runCountMulsOptimized :: Int -> Int -> Int
runCountNonPrimesOptimized a b = length . filter isPrime $ ns where
  ns = [a,a+17..b]
  isPrime n = any ((==) 0 . mod n) fs where
    fs = [2..maxF] :: [Int]
    maxF = ceiling (sqrt $ fromIntegral n)

runCountMulsOptimized a b = sum . map countMuls $ ns where
  ns = [a,a+17..b]
  countMuls n = length [1| a <- ns', b <- ns'] where
    ns' = [2..(n-1)]