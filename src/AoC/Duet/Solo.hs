module AoC.Duet.Solo (
  ExecutionContext(..),
  stepOnce, runSoloFirstRcv, emptyExecutionContext) where

import AoC.Focus.List
import AoC.Duet.Core

data ExecutionContext = ExecutionContext Cpu Int
  deriving (Eq)

instance Show ExecutionContext where
  show (ExecutionContext (Cpu rs is) hz) = "ËxecutionContext:\nCpu " ++ show rs ++ " " ++ show hz ++ "\n" ++ show' is where
    show' :: Program -> String
    show' (Program l) = unlines $ (map ((++) "  ") . map show . prefix $ l) ++ ["> " ++ show (get l)] ++ (map ((++) "  ") . map show . postfix $ l)

emptyExecutionContext :: [Instruction] -> ExecutionContext
emptyExecutionContext is = ExecutionContext (Cpu (Registers []) (makeProgram is)) 0

withCpu :: (Cpu -> Cpu) -> ExecutionContext -> ExecutionContext
withCpu f (ExecutionContext c hz) = ExecutionContext (f c) hz

stepOnce :: ExecutionContext -> ExecutionContext
stepOnce ec =
  executeInstruction instr ec where
    instr = nextInstruction . cpu $ ec

runSoloFirstRcv :: ExecutionContext -> ExecutionContext
runSoloFirstRcv ec = if isRcv . nextInstruction . cpu $ ec then ec' else runSoloFirstRcv ec' where
  ec' = stepOnce ec
  isRcv (Rcv reg) = 0 /= (getRegister reg . cpu $ ec)
  isRcv _ = False

cpu :: ExecutionContext -> Cpu
cpu (ExecutionContext cpu _) = cpu

executeInstruction :: Instruction -> ExecutionContext -> ExecutionContext
executeInstruction (Snd hzVal) ec = withCpu jumpNext . setOutput hz $ ec where
  hz = getValue hzVal $ cpu ec
executeInstruction (Rcv reg) ec@(ExecutionContext c hz) =
  if getRegister reg (cpu ec) /= 0
  then withCpu (jumpNext . (setRegister reg hz)) $ ec
  else withCpu jumpNext $ ec
executeInstruction i ec = withCpu (executeCpuInstruction i) $ ec


setOutput :: Int -> ExecutionContext -> ExecutionContext
setOutput hz (ExecutionContext cpu _) = ExecutionContext cpu hz

getOutput :: ExecutionContext -> Int
getOutput (ExecutionContext _ hz) = hz
  