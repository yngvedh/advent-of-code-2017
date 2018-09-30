module AoC.Duet.Duo (emptyExecutionContext, run, swapCpus, runActiveCpu, stepOnce, activeSolo, soloByName, ExecutionContext(..)) where

import AoC.Duet.Core
import AoC.Duet.Channel
import qualified AoC.Duet.Solo as S

data ExecutionContext a = ExecutionContext (String ,S.ExecutionContext a) (String, S.ExecutionContext a)
  deriving (Show, Eq)

emptyExecutionContext :: (S.Channel a) => [Instruction] -> ExecutionContext a
emptyExecutionContext is = ExecutionContext (namedEc is 0) (namedEc is 1) where
  namedEc is n = (show n, S.preparedExecutionContext is n)

run :: (S.Channel a, Eq a) => ExecutionContext a -> ExecutionContext a
run ec = if ec == ec' then ec else run ec' where
  ec' = swapCpus . stepOnce . swapCpus . stepOnce $ ec

runActiveCpu, swapCpus :: (S.Channel a, Eq a) => ExecutionContext a -> ExecutionContext a
runActiveCpu ec = if ec == ec' then ec else runActiveCpu ec' where
  ec' = stepOnce ec
swapCpus (ExecutionContext a b) = ExecutionContext b a

stepOnce :: (Eq a, S.Channel a) => ExecutionContext a -> ExecutionContext a
stepOnce ec = executeInstruction instr ec where
    instr = S.nextInstruction . activeCpu $ ec

activeSolo (ExecutionContext (_, solo) _) = solo
activeCpu = S.cpu . activeSolo

executeInstruction :: (S.Channel a, Eq a) => Instruction -> ExecutionContext a -> ExecutionContext a
executeInstruction i@(Snd val) (ExecutionContext (n, ec) (n2, ec2)) = ExecutionContext (n, ec') (n2, ec2') where
  v = getValue val . S.cpu $ ec
  ec' = S.logInstr i . S.withCpu jumpNext $ ec
  ec2' = S.setOutput v ec2
executeInstruction i@(Rcv reg) e@(ExecutionContext (n, ec@(S.ExecutionContext cpu c log)) (n2, ec2)) =
  ExecutionContext (n, ec') (n2, ec2) where
  wait = not . hasInput . S.channel $ ec
  (input, c') = channelRead . S.channel $ ec
  ec' = if wait
    then S.logWait ec
    else S.logInstr i $ S.ExecutionContext (jumpNext . setRegister reg input $ cpu) c' log

executeInstruction i (ExecutionContext (n, ec) ec') = ExecutionContext (n, S.executeInstruction i ec) ec'

soloByName :: String -> ExecutionContext a -> S.ExecutionContext a
soloByName name (ExecutionContext (n,s) (n',s')) = if n == name then s else s'