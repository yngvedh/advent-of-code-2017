module AoC.Duet.Duo (
  ExecutionContext(..),
  emptyExecutionContext,
  swapCpus, stepOnce, stepDuoOnce,
  run, runActiveCpu,
  activeSolo, soloByName) where

import AoC.Duet.Core
import AoC.Duet.Channel
import qualified AoC.Duet.Solo as S

data ExecutionContext a = ExecutionContext (String, S.ExecutionContext a) (String, S.ExecutionContext a)
  deriving (Show, Eq)

emptyExecutionContext :: (S.Channel a) => [Instruction] -> ExecutionContext a
emptyExecutionContext is = ExecutionContext (namedEc is 0) (namedEc is 1) where
  namedEc is n = (show n, S.preparedExecutionContext is n)

run, stepDuoOnce :: (S.Channel a, Eq a) => ExecutionContext a -> ExecutionContext a
run ec = let
  ec' = stepDuoOnce ec
  in if hasHalted ec'
    then ec'
    else run ec'
  
stepDuoOnce = stepOnce . swapCpus . stepOnce . swapCpus

runActiveCpu, swapCpus :: (S.Channel a, Eq a) =>
  ExecutionContext a ->
  ExecutionContext a
runActiveCpu ec = let
  ec' = stepOnce ec
  in if hasHalted ec'
    then ec'
    else runActiveCpu ec'

hasHalted :: ExecutionContext a -> Bool
hasHalted = halted . lastEntries where
  lastEntries :: ExecutionContext a -> (S.LogEntry, S.LogEntry) 
  lastEntries (ExecutionContext (n1, s1) (n2, s2)) = (lastEntry s1, lastEntry s2)
  lastEntry = S.lastEntry . S.executionLog
  halted :: (S.LogEntry, S.LogEntry) -> Bool
  halted (_, S.Halt) = True
  halted (S.Halt, _) = True
  halted (S.Wait, S.Wait) = True
  halted _ = False

swapCpus (ExecutionContext a b) = ExecutionContext b a

stepOnce :: (Eq a, S.Channel a) => ExecutionContext a -> ExecutionContext a
stepOnce ec = executeInstruction instr ec where
    instr = S.nextInstruction . activeCpu $ ec

activeSolo (ExecutionContext (_, solo) _) = solo
activeCpu = S.cpu . activeSolo

executeInstruction :: (S.Channel a, Eq a) =>
  Instruction ->
  ExecutionContext a ->
  ExecutionContext a
executeInstruction i@(Snd val) ec@(ExecutionContext (n1, ec1) (n2, ec2)) =
  ExecutionContext (n1, ec1') (n2, ec2') where
    v = getValue val . S.cpu $ ec1
    ec' = withActiveExecutionContext (S.withCpuAndLog i jumpNext) ec
    ec1' = activeSolo ec'
    ec2' = S.setOutput v ec2

executeInstruction i@(Rcv reg) ec =
  withActiveExecutionContext rcv ec where
    rcv :: (S.Channel a, Eq a) => S.ExecutionContext a -> S.ExecutionContext a
    rcv ec1 =
      if wait
        then S.logWait $ ec1
        else S.withCpuAndLog i (jumpNext . setRegister reg input) $ S.setChannel c' ec1
      where
        wait = not . hasInput . S.channel $ ec1
        (input, c') = channelRead . S.channel $ ec1

executeInstruction i ec = 
  withActiveExecutionContext (S.executeInstruction i) ec

withActiveExecutionContext ::
  (S.ExecutionContext a -> S.ExecutionContext a) ->
  ExecutionContext a ->
  ExecutionContext a
withActiveExecutionContext f ec@(ExecutionContext (n1, ec1) (n2, ec2)) =
  ExecutionContext (n1, ec1') (n2, ec2) where
    ec1' = f ec1
      
soloByName :: String -> ExecutionContext a -> S.ExecutionContext a
soloByName name (ExecutionContext (n,s) (n',s')) = if n == name then s else s'