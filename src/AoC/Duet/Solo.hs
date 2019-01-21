module AoC.Duet.Solo (
  ExecutionContext(..), Channel, IoReg(..), IoBuf(..), ExecutionLog(..), LogEntry(..),
  stepOnce, runSoloFirstRcv, emptyExecutionContext, preparedExecutionContext, runSolo,
  executeInstruction, cpu, channel, nextInstruction, withCpu, setOutput, withCpuAndLog, setChannel,
  makeExecutionLog, executionLog,
  logWait, logSkip, logInstr,
  getEitherLog, showInstr, showLogEntry) where

import Data.List (intersperse)
import qualified Data.Text as T

import AoC.Focus.List
import AoC.Duet.Core
import AoC.Duet.Channel

data LogEntry = Wait | Skip | Exe Instruction
    deriving (Show, Eq)

showLogEntry Skip = "Skip"
showLogEntry Wait = "Wait"
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

withCpu :: (Cpu -> Maybe Cpu) -> ExecutionContext a -> Either ExecutionLog (ExecutionContext a)
withCpu f (ExecutionContext c hz log) = case f c of
  Just c' -> Right $ ExecutionContext c' hz log
  _       -> Left log

withLog :: (ExecutionLog -> ExecutionLog) -> ExecutionContext a -> ExecutionContext a
withLog f (ExecutionContext cpu ch log) = ExecutionContext cpu ch (f log)

stepOnce :: (Eq a, Channel a) => ExecutionContext a -> Either ExecutionLog (ExecutionContext a)
stepOnce ec =
  executeInstruction instr ec where
    instr = nextInstruction . cpu $ ec

runSoloFirstRcv :: (Eq a, Channel a) => ExecutionContext a -> Either ExecutionLog (ExecutionContext a)
runSoloFirstRcv ec = do
  ec' <- stepOnce ec
  if hasReceived ec'
    then return ec'
    else runSoloFirstRcv ec'

hasReceived :: ExecutionContext a -> Bool
hasReceived ec = ((<) 0) . numRcvs . executionLog $ ec where

runSolo :: (Eq a, Channel a, Show a) => ExecutionContext a -> Either ExecutionLog (ExecutionContext a)
runSolo ec = do
  ec' <- stepOnce ec
  if ec == ec'
    then return ec
    else runSolo ec'

cpu :: ExecutionContext a -> Cpu
cpu (ExecutionContext cpu _ _) = cpu

channel :: ExecutionContext a -> a
channel (ExecutionContext _ c _) = c

executionLog :: ExecutionContext a -> ExecutionLog
executionLog (ExecutionContext _ _ l) = l

executeInstruction :: (Channel a, Eq a)
  => Instruction
  -> ExecutionContext a
  -> Either ExecutionLog (ExecutionContext a)
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

executeInstruction i ec =
  withFailingCpu (executeCpuInstruction i) $ logInstr i ec

withFailingCpu :: (Cpu -> Maybe Cpu) -> ExecutionContext a -> Either ExecutionLog (ExecutionContext a)
withFailingCpu f ec@(ExecutionContext cpu ch log) =
  case f cpu of
    Just cpu' -> Right $ ExecutionContext cpu' ch log
    Nothing   -> Left . executionLog $ ec

withCpuAndLog :: 
  Instruction ->
  (Cpu -> Maybe Cpu) ->
  ExecutionContext a ->
  Either ExecutionLog (ExecutionContext a)
withCpuAndLog i f ec = withFailingCpu f ec >>= return . logInstr i
  
setOutput :: (Channel a) => Int -> ExecutionContext a -> ExecutionContext a
setOutput hz (ExecutionContext cpu ch log) = ExecutionContext cpu (channelWrite hz ch) log

getOutput :: (Channel a) => ExecutionContext a -> (Int, ExecutionContext a)
getOutput (ExecutionContext cpu ch log) = (hz, ExecutionContext cpu ch' log) where
  (hz, ch') = channelRead ch

setChannel :: (Channel a) => a -> ExecutionContext a -> ExecutionContext a
setChannel ch (ExecutionContext cpu _ log) = ExecutionContext cpu ch log

logInstr :: Instruction -> ExecutionContext a -> ExecutionContext a
logInstr = appendLog . Exe

logWait, logSkip :: ExecutionContext a -> ExecutionContext a
logWait = appendLog Wait
logSkip = appendLog Skip

appendLog :: LogEntry -> ExecutionContext a -> ExecutionContext a
appendLog e = withLog $ traceInstr . setLast where
  setLast log = log { lastEntry = e }
  traceInstr log = case e of
    (Exe (Mul _ _)) -> log { numMuls = 1 + numMuls log }
    (Exe (Rcv _))   -> log { numRcvs = 1 + numRcvs log }
    (Exe (Snd _))   -> log { numSnds = 1 + numSnds log }
    _               -> log

getEitherLog :: Either ExecutionLog (ExecutionContext a) -> ExecutionLog
getEitherLog (Left log) = log
getEitherLog (Right ec) = executionLog ec