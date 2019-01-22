module AoC.Duet.Solo (
  ExecutionContext(..), Channel, IoReg(..), IoBuf(..), ExecutionLog(..), LogEntry(..),
  stepOnce, runSoloFirstRcv, emptyExecutionContext, preparedExecutionContext, runSolo,
  executeInstruction, cpu, channel, nextInstruction, withCpu, setOutput, withCpuAndLog, setChannel,
  makeExecutionLog, executionLog,
  logWait, logSkip, logHalt, logInstr,
  showInstr, showLogEntry) where

import Data.List (intersperse)
import qualified Data.Text as T

import AoC.Focus.List
import AoC.Duet.Core
import AoC.Duet.Channel

data LogEntry = Exe Instruction | Wait | Skip | Halt
    deriving (Show, Eq)

showLogEntry Skip = "Skip"
showLogEntry Wait = "Wait"
showLogEntry Halt = "Halt"
showLogEntry (Exe i) = showInstr i

data ExecutionLog = ExecutionLog {
  lastEntry :: !LogEntry,
  numMuls :: !Int,
  numRcvs :: !Int,
  numSnds :: !Int
} deriving (Show, Eq)

data ExecutionContext a = ExecutionContext Cpu a !ExecutionLog

instance (Eq a) => Eq (ExecutionContext a) where
  (==) (ExecutionContext cpu1 ch1 _) (ExecutionContext cpu2 ch2 _) = cpu1 == cpu2 && ch1 == ch2

instance (Show a) => Show (ExecutionContext a) where
  show (ExecutionContext (Cpu rs is) hz log) = "ExecutionContext:\nCpu " ++ show rs ++ " " ++ show hz ++ "\n" ++ show' is where
    show' :: Program -> String
    show' (Program l) = concat $ (intersperse " " . map showInstr . prefix $ l) ++ ["[" ++ showInstr (get l) ++ "]"] ++ (intersperse " " . map showInstr . postfix $ l)
    show' OutOfBounds = "[out of bounds]"

showInstr (Snd v) = "Snd" ++ showVal v
showInstr (Set r v) = "Set" ++ showReg r ++ showVal v
showInstr (Add r v) = "Add" ++ showReg r ++ showVal v
showInstr (Mul r v) = "Mul" ++ showReg r ++ showVal v
showInstr (Mod r v) = "Mod" ++ showReg r ++ showVal v
showInstr (Rcv r) = "Rcv" ++ showReg r
showInstr (Jgz r v) = "Jgz" ++ showVal r ++ showVal v
showInstr (Jnz r v) = "Jnz" ++ showVal r ++ showVal v
showReg (Register name) = "," ++ show name
showVal (LiteralValue v) = "," ++ show v
showVal (RegisterValue r) = showReg r

makeExecutionLog = ExecutionLog { lastEntry = Skip, numMuls = 0, numRcvs = 0, numSnds = 0 }

emptyExecutionContext :: (Channel a) => [Instruction] -> ExecutionContext a
emptyExecutionContext is = ExecutionContext (Cpu makeRegisters (makeProgram is)) makeChannel makeExecutionLog

preparedExecutionContext :: (Channel a) => [Instruction] -> Int -> ExecutionContext a
preparedExecutionContext is v = 
  ExecutionContext
    (Cpu (makeRegistersFromList [(Register $ T.pack "p", v)]) (makeProgram is))
    makeChannel
    makeExecutionLog

withCpu :: (Cpu -> Cpu) -> ExecutionContext a -> ExecutionContext a
withCpu f ec@(ExecutionContext c hz log) = if isFaulted c' then logHalt ec' else ec' where
  c' = f c
  ec' = ExecutionContext c' hz log

withLog :: (ExecutionLog -> ExecutionLog) -> ExecutionContext a -> ExecutionContext a
withLog f (ExecutionContext cpu ch log) = ExecutionContext cpu ch (f log)

stepOnce :: (Eq a, Channel a) => ExecutionContext a -> ExecutionContext a
stepOnce ec = executeInstruction instr ec
  where instr = nextInstruction . cpu $ ec

runSoloFirstRcv :: (Eq a, Channel a) => ExecutionContext a -> ExecutionContext a
runSoloFirstRcv ec = let
  ec' = stepOnce ec
  in if hasReceived ec'
    then ec'
    else runSoloFirstRcv ec'

hasReceived :: ExecutionContext a -> Bool
hasReceived ec = ((<) 0) . numRcvs . executionLog $ ec where

runSolo :: (Eq a, Channel a, Show a) => ExecutionContext a -> ExecutionContext a
runSolo ec = let
  ec' = stepOnce ec
  in case lastEntry $ executionLog ec' of
    Halt -> ec'
    Wait -> ec'
    Skip -> ec'
    _    -> runSolo ec'

cpu :: ExecutionContext a -> Cpu
cpu (ExecutionContext cpu _ _) = cpu

channel :: ExecutionContext a -> a
channel (ExecutionContext _ c _) = c

executionLog :: ExecutionContext a -> ExecutionLog
executionLog (ExecutionContext _ _ l) = l

executeInstruction :: (Channel a, Eq a) => Instruction -> ExecutionContext a -> ExecutionContext a
executeInstruction i@(Snd hzVal) ec =
  let hz = getValue hzVal $ cpu ec
  in withCpu jumpNext . setOutput hz . logInstr i $ ec
executeInstruction i@(Rcv reg) ec@(ExecutionContext cpu ch log) =
  if getRegister reg cpu /= 0
  then
    let (hz, ec') = getOutput . logInstr i $ ec
    in withCpu (jumpNext . (setRegister reg hz)) ec'
  else
    withCpu jumpNext . logSkip $ ec

executeInstruction i ec = withCpu (executeCpuInstruction i) $ logInstr i ec

withCpuAndLog :: Instruction -> (Cpu -> Cpu) -> ExecutionContext a -> ExecutionContext a
withCpuAndLog i f ec = logInstr i . withCpu f $ ec
  
setOutput :: (Channel a) => Int -> ExecutionContext a -> ExecutionContext a
setOutput hz (ExecutionContext cpu ch log) = ExecutionContext cpu (channelWrite hz ch) log

getOutput :: (Channel a) => ExecutionContext a -> (Int, ExecutionContext a)
getOutput (ExecutionContext cpu ch log) = (hz, ExecutionContext cpu ch' log) where
  (hz, ch') = channelRead ch

setChannel :: (Channel a) => a -> ExecutionContext a -> ExecutionContext a
setChannel ch (ExecutionContext cpu _ log) = ExecutionContext cpu ch log

logInstr :: Instruction -> ExecutionContext a -> ExecutionContext a
logInstr = appendLog . Exe

logWait, logSkip, logHalt :: ExecutionContext a -> ExecutionContext a
logWait = appendLog Wait
logSkip = appendLog Skip
logHalt = appendLog Halt

appendLog :: LogEntry -> ExecutionContext a -> ExecutionContext a
appendLog e = withLog $ traceInstr . setLast where
  setLast log = log { lastEntry = e }
  traceInstr log = case e of
    (Exe (Mul _ _)) -> log { numMuls = 1 + numMuls log }
    (Exe (Rcv _))   -> log { numRcvs = 1 + numRcvs log }
    (Exe (Snd _))   -> log { numSnds = 1 + numSnds log }
    _               -> log
