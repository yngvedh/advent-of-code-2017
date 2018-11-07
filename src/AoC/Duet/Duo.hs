module AoC.Duet.Duo (
  ExecutionContext(..), ExecutionLogs(..),
  emptyExecutionContext,
  swapCpus, stepOnce, stepDuoOnce,
  run, runActiveCpu,
  activeSolo, soloByName) where

import AoC.Duet.Core
import AoC.Duet.Channel
import qualified AoC.Duet.Solo as S

data ExecutionContext a = ExecutionContext (String ,S.ExecutionContext a) (String, S.ExecutionContext a)
  deriving (Show, Eq)

data ExecutionLogs = ExecutionLogs {
  executionLog1 :: S.ExecutionLog,
  executionLog2 :: S.ExecutionLog
}
  deriving (Show, Eq)

emptyExecutionContext :: (S.Channel a) => [Instruction] -> ExecutionContext a
emptyExecutionContext is = ExecutionContext (namedEc is 0) (namedEc is 1) where
  namedEc is n = (show n, S.preparedExecutionContext is n)

run, stepDuoOnce :: (S.Channel a, Eq a) => ExecutionContext a -> Either ExecutionLogs (ExecutionContext a)
run ec = do
  ec' <- stepDuoOnce ec
  if ec == ec'
    then return ec
    else run ec'
  
stepDuoOnce ec = return ec >>= swapCpus >>= stepOnce >>= swapCpus >>= stepOnce

runActiveCpu, swapCpus :: (S.Channel a, Eq a) =>
  ExecutionContext a ->
  Either ExecutionLogs (ExecutionContext a)
runActiveCpu ec = do
  ec' <- stepOnce ec
  if ec == ec'
    then return ec
    else runActiveCpu ec'

swapCpus (ExecutionContext a b) = Right $ ExecutionContext b a

stepOnce :: (Eq a, S.Channel a) => ExecutionContext a -> Either ExecutionLogs (ExecutionContext a)
stepOnce ec = executeInstruction instr ec where
    instr = S.nextInstruction . activeCpu $ ec

activeSolo (ExecutionContext (_, solo) _) = solo
activeCpu = S.cpu . activeSolo

executeInstruction :: (S.Channel a, Eq a) =>
  Instruction ->
  ExecutionContext a ->
  Either ExecutionLogs (ExecutionContext a)
executeInstruction i@(Snd val) ec@(ExecutionContext (n1, ec1) (n2, ec2)) = do
  let v = getValue val . S.cpu $ ec1
  ec' <- withActiveExecutionContext (S.withCpuAndLog i jumpNext) ec
  let ec1' = activeSolo ec'
  let ec2' = S.setOutput v ec2
  return $ ExecutionContext (n1, ec1') (n2, ec2')

executeInstruction i@(Rcv reg) ec =
  withActiveExecutionContext rcv ec where
    rcv :: (S.Channel a, Eq a) => S.ExecutionContext a -> Either S.ExecutionLog (S.ExecutionContext a)
    rcv ec1 =
      if wait
        then Right . S.logWait $ ec1
        else S.withCpuAndLog i (jumpNext . setRegister reg input) $ S.setChannel c' ec1
      where
        wait = not . hasInput . S.channel $ ec1
        (input, c') = channelRead . S.channel $ ec1

executeInstruction i ec = 
  withActiveExecutionContext (S.executeInstruction i) ec

withActiveExecutionContext ::
  (S.ExecutionContext a -> Either S.ExecutionLog (S.ExecutionContext a)) ->
  ExecutionContext a ->
  Either ExecutionLogs (ExecutionContext a)
withActiveExecutionContext f ec@(ExecutionContext (n1, ec1) (n2, ec2)) =
  case f ec1 of
    Right ec1' -> Right $ ExecutionContext (n1, ec1') (n2, ec2)
    Left logA  -> Left $ ExecutionLogs { executionLog1 = log1, executionLog2 = log2 } where
      logB = S.executionLog ec2
      log1 = if n1 == "0" then logA else logB
      log2 = if n1 == "0" then logB else logA
      
soloByName :: String -> ExecutionContext a -> S.ExecutionContext a
soloByName name (ExecutionContext (n,s) (n',s')) = if n == name then s else s'