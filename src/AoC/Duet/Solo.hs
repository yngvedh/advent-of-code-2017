module AoC.Duet.Solo (
  ExecutionContext(..), Channel, IoReg(..), IoBuf(..), ExecutionLog(..), LogEntry(..),
  stepOnce, runSoloFirstRcv, emptyExecutionContext, preparedExecutionContext,
  executeInstruction, cpu, channel, nextInstruction, withCpu, setOutput, withCpuAndLog, setChannel,
  makeExecutionLog, executionLog, logWait, logSkip, logInstr, getLog, showInstr, showLogEntry) where

import Data.List (intersperse)
import Debug.Trace (trace)

import AoC.Focus.List
import AoC.Duet.Core
import AoC.Duet.Channel

data LogEntry = Wait | Skip | Exe Instruction
    deriving (Show, Eq)

showLogEntry Skip = "Skip"
showLogEntry Wait = "Wait"
showLogEntry (Exe i) = showInstr i

data ExecutionLog = ExecutionLog [LogEntry]
    deriving (Show, Eq)

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
hasReceived ec = any isRcv . getLog . executionLog $ ec where
  isRcv (Exe (Rcv _)) = True
  isRcv _ = False

runSolo :: (Eq a, Channel a) => ExecutionContext a -> Either ExecutionLog (ExecutionContext a)
runSolo ec = do
  ec' <- stepOnce ec
  runSolo ec'
--  runSolo . stepOnce $ ec

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
executeInstruction i@(Snd hzVal) ec = do
  let hz = getValue hzVal $ cpu ec
  ec' <- withCpu jumpNext . setOutput hz $ ec
  return $ logInstr i ec'
executeInstruction i@(Rcv reg) ec@(ExecutionContext cpu ch log) =
  if getRegister reg cpu /= 0
  then do
    let (hz, ec') = getOutput ec
    ec'' <- withCpu (jumpNext . (setRegister reg hz)) ec'
    return $ logInstr i ec''
  else
    withCpu jumpNext ec >>= Right . logSkip

--    ec' <- withCpu jumpNext ec
--    return $ logSkip ec'

--    ec >>= (withCpu jumpNext) >>= (return . logSkip)
executeInstruction i ec = do
  ec' <- withFailingCpu (executeCpuInstruction i) $ ec
  return  $ logInstr i ec'

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
appendLog e = withLog append where
  append (ExecutionLog is) = ExecutionLog (e:is)

getLog :: ExecutionLog -> [LogEntry]
getLog (ExecutionLog is) = is