module AoC.Duet.Solo (
  ExecutionContext(..), Channel, IoReg(..), IoBuf(..),
  stepOnce, runSoloFirstRcv, emptyExecutionContext) where

import AoC.Focus.List
import AoC.Duet.Core

data ExecutionContext a = ExecutionContext Cpu a
  deriving (Eq)

class Channel a where
  channelRead :: a -> (Int , a)
  channelWrite :: Int -> a -> a
  makeChannel :: a

instance Channel IoReg where
  channelRead = channelRegRead
  channelWrite = channelRegWrite
  makeChannel = IoReg 0

instance Channel IoBuf where
  channelRead = channelBufRead
  channelWrite = channelBufWrite
  makeChannel = IoBuf []

data IoReg = IoReg Int
  deriving (Show, Eq)

channelRegRead :: IoReg -> (Int, IoReg)
channelRegRead r@(IoReg n) = (n, r)

channelRegWrite :: Int -> IoReg -> IoReg
channelRegWrite n _ = IoReg n

data IoBuf = IoBuf [Int]
  deriving (Show, Eq)

channelBufRead :: IoBuf -> (Int, IoBuf)
channelBufRead (IoBuf (n:ns)) = (n, IoBuf ns)

channelBufWrite :: Int -> IoBuf -> IoBuf
channelBufWrite n (IoBuf ns) = IoBuf (n:ns)

instance (Show a) => Show (ExecutionContext a) where
  show (ExecutionContext (Cpu rs is) hz) = "ExecutionContext:\nCpu " ++ show rs ++ " " ++ show hz ++ "\n" ++ show' is where
    show' :: Program -> String
    show' (Program l) = unlines $ (map ((++) "  ") . map show . prefix $ l) ++ ["> " ++ show (get l)] ++ (map ((++) "  ") . map show . postfix $ l)

emptyExecutionContext :: (Channel a) => [Instruction] -> ExecutionContext a
emptyExecutionContext is = ExecutionContext (Cpu (Registers []) (makeProgram is)) makeChannel

withCpu :: (Cpu -> Cpu) -> ExecutionContext a -> ExecutionContext a
withCpu f (ExecutionContext c hz) = ExecutionContext (f c) hz

stepOnce :: (Eq a, Channel a) => ExecutionContext a -> ExecutionContext a
stepOnce ec =
  executeInstruction instr ec where
    instr = nextInstruction . cpu $ ec

runSoloFirstRcv :: (Eq a, Channel a) => ExecutionContext a -> ExecutionContext a
runSoloFirstRcv ec = if isRcv . nextInstruction . cpu $ ec then ec' else runSoloFirstRcv ec' where
  ec' = stepOnce ec
  isRcv (Rcv reg) = 0 /= (getRegister reg . cpu $ ec)
  isRcv _ = False

cpu :: ExecutionContext a -> Cpu
cpu (ExecutionContext cpu _) = cpu

executeInstruction :: (Channel a, Eq a) => Instruction -> ExecutionContext a -> ExecutionContext a
executeInstruction (Snd hzVal) ec = withCpu jumpNext . setOutput hz $ ec where
  hz = getValue hzVal $ cpu ec
executeInstruction (Rcv reg) ec@(ExecutionContext cpu ch) =
  if getRegister reg cpu /= 0
  then let (hz, ec') = getOutput ec in
    withCpu (jumpNext . (setRegister reg hz)) $ ec'
  else withCpu jumpNext $ ec
executeInstruction i ec = withCpu (executeCpuInstruction i) $ ec

setOutput :: (Channel a) => Int -> ExecutionContext a -> ExecutionContext a
setOutput hz (ExecutionContext cpu ch) = ExecutionContext cpu (channelWrite hz ch)

getOutput :: (Channel a) => ExecutionContext a -> (Int, ExecutionContext a)
getOutput (ExecutionContext cpu ch) = (hz, ExecutionContext cpu ch') where
  (hz, ch') = channelRead ch
  