module AoC.Duet.Solo (
  ExecutionContext(..), Channel, IoReg(..), IoBuf(..), ExecutionLog(..), LogEntry(..),
  stepOnce, runSoloFirstRcv, emptyExecutionContext, preparedExecutionContext,
  executeInstruction, cpu, channel, nextInstruction, withCpu, setOutput,
  makeExecutionLog, executionLog, logWait, logSkip, logInstr, getLog, showInstr, showLogEntry) where

import Data.List (intersperse)
import Debug.Trace (trace)

import AoC.Focus.List
import AoC.Duet.Core
import AoC.Duet.Channel

data LogEntry = Wait | Skip | Exe Instruction
    deriving (Eq)

showLogEntry Skip = "Skip"
showLogEntry Wait = "Wait"
showLogEntry (Exe i) = showInstr i

data ExecutionLog = ExecutionLog [LogEntry]
    deriving (Eq)

data ExecutionContext a = ExecutionContext Cpu a ExecutionLog

instance (Eq a) => Eq (ExecutionContext a) where
  (==) (ExecutionContext cpu1 ch1 _) (ExecutionContext cpu2 ch2 _) = cpu1 == cpu2 && ch1 == ch2

instance (Show a) => Show (ExecutionContext a) where
  show (ExecutionContext (Cpu rs is) hz log) = "ExecutionContext:\nCpu " ++ show rs ++ " " ++ show hz ++ "\n" ++ show' is where
    show' :: Program -> String
    show' (Program l) = concat $ (intersperse " " . map showInstr . prefix $ l) ++ ["[" ++ showInstr (get l) ++ "]"] ++ (intersperse " " . map showInstr . postfix $ l)

showInstr (Snd v) = "Snd" ++ showVal v
showInstr (Set r v) = "Set" ++ showReg r ++ showVal v
showInstr (Add r v) = "Add" ++ showReg r ++ showVal v
showInstr (Mul r v) = "Mul" ++ showReg r ++ showVal v
showInstr (Mod r v) = "Mod" ++ showReg r ++ showVal v
showInstr (Rcv r) = "Rcv" ++ showReg r
showInstr (Jgz r v) = "Jgz" ++ showVal r ++ showVal v
showReg (Register name) = "," ++ name
showVal (LiteralValue v) = "," ++ show v
showVal (RegisterValue r) = showReg r

makeExecutionLog = ExecutionLog []

emptyExecutionContext :: (Channel a) => [Instruction] -> ExecutionContext a
emptyExecutionContext is = ExecutionContext (Cpu makeRegisters (makeProgram is)) makeChannel makeExecutionLog

preparedExecutionContext :: (Channel a) => [Instruction] -> Int -> ExecutionContext a
preparedExecutionContext is v = ExecutionContext (Cpu (makeRegistersFromList [(Register "p", v)]) (makeProgram is)) makeChannel makeExecutionLog

withCpu :: (Cpu -> Cpu) -> ExecutionContext a -> ExecutionContext a
withCpu f (ExecutionContext c hz log) = ExecutionContext (f c) hz log

withLog :: (ExecutionLog -> ExecutionLog) -> ExecutionContext a -> ExecutionContext a
withLog f (ExecutionContext cpu ch log) = ExecutionContext cpu ch (f log)

stepOnce :: (Eq a, Channel a) => ExecutionContext a -> ExecutionContext a
stepOnce ec =
  executeInstruction instr ec where
    instr = nextInstruction . cpu $ ec

runSoloFirstRcv :: (Eq a, Channel a) => ExecutionContext a -> ExecutionContext a
runSoloFirstRcv ec = if hasReceived then ec' else runSoloFirstRcv ec' where
  ec' = stepOnce ec
  hasReceived = any isRcv . getLog . executionLog $ ec' where
    isRcv (Exe (Rcv _)) = True
    isRcv _ = False

cpu :: ExecutionContext a -> Cpu
cpu (ExecutionContext cpu _ _) = cpu

channel :: ExecutionContext a -> a
channel (ExecutionContext _ c _) = c

executionLog :: ExecutionContext a -> ExecutionLog
executionLog (ExecutionContext _ _ l) = l

executeInstruction :: (Channel a, Eq a) => Instruction -> ExecutionContext a -> ExecutionContext a
executeInstruction i@(Snd hzVal) ec = logInstr i . withCpu jumpNext . setOutput hz $ ec where
  hz = getValue hzVal $ cpu ec
executeInstruction i@(Rcv reg) ec@(ExecutionContext cpu ch log) =
  if getRegister reg cpu /= 0
  then let (hz, ec') = getOutput ec in
    logInstr i . withCpu (jumpNext . (setRegister reg hz)) $ ec'
  else logSkip . withCpu jumpNext $ ec
executeInstruction i ec = logInstr i . withCpu (executeCpuInstruction i) $ ec

setOutput :: (Channel a) => Int -> ExecutionContext a -> ExecutionContext a
setOutput hz (ExecutionContext cpu ch log) = ExecutionContext cpu (channelWrite hz ch) log

getOutput :: (Channel a) => ExecutionContext a -> (Int, ExecutionContext a)
getOutput (ExecutionContext cpu ch log) = (hz, ExecutionContext cpu ch' log) where
  (hz, ch') = channelRead ch


logInstr = appendLog . Exe
logWait = appendLog Wait
logSkip = appendLog Skip

appendLog :: LogEntry -> ExecutionContext a -> ExecutionContext a
appendLog e = withLog append where
  append (ExecutionLog is) = ExecutionLog (e:is)

getLog :: ExecutionLog -> [LogEntry]
getLog (ExecutionLog is) = is