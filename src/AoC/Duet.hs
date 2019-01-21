module AoC.Duet (
  parseDuetProgram,
  makeSoloExecutionContext, runSoloFirstRcv, soloOutputFrequency,
  makeDuo, runDuo, ec1reads, stepDuo,
  showDuo, showSolo
  ) where

import qualified AoC.Duet.Core as C
import qualified AoC.Duet.Parse as P
import qualified AoC.Duet.Solo as S
import qualified AoC.Duet.Duo as D
import qualified AoC.Duet.Channel as Ch
import qualified AoC.Focus.List as F

parseDuetProgram = P.parseInstructions
makeSoloExecutionContext :: [C.Instruction] -> S.ExecutionContext S.IoReg
makeSoloExecutionContext = S.emptyExecutionContext
runSoloFirstRcv :: (Eq a, S.Channel a) =>
  S.ExecutionContext a ->
  Either S.ExecutionLog (S.ExecutionContext a)
runSoloFirstRcv = S.runSoloFirstRcv

soloOutputFrequency :: Either S.ExecutionLog (S.ExecutionContext S.IoReg) -> Int
soloOutputFrequency (Right (S.ExecutionContext _ ch _)) = fst . Ch.channelRead $ ch
soloOutputFrequency (Left _) = error "soloOutputFrequency called on halted cpu"

runDuo, stepDuo :: (Ch.Channel a, Eq a) =>
  D.ExecutionContext a ->
  Either D.ExecutionLogs (D.ExecutionContext a)
runDuo = D.run
stepDuo = D.stepDuoOnce

makeDuo :: [C.Instruction] -> D.ExecutionContext Ch.IoBuf
makeDuo = D.emptyExecutionContext

ec1reads :: (Ch.Channel a) => Either D.ExecutionLogs (D.ExecutionContext a) -> Int
ec1reads = S.numRcvs . getLog where
  getLog :: (Ch.Channel a) => Either D.ExecutionLogs (D.ExecutionContext a) -> S.ExecutionLog
  getLog (Right ec) = S.executionLog . D.soloByName "0" $ ec
  getLog (Left logs) = D.executionLog1 $ logs

showSolo :: (Show a) => S.ExecutionContext a -> String
showSolo (S.ExecutionContext cpu@(C.Cpu regs prog) ch log) =
  unlines $ ["Registers:"] ++ registers ++ ["Channel: " ++ show ch] ++ ["Program"] ++ program where
    registers = map ((++) "  " . showRegister) $ C.registerNames regs
    program = pre ++ [cur] ++ post
    pre = map ((++) "  " . S.showInstr) $ F.prefix focus
    cur = (++) "> " . S.showInstr . F.get $ focus
    post = map ((++) "  " . S.showInstr) $ F.postfix focus
    focus = C.instructions prog 
    showRegister :: C.Register -> String
    showRegister reg@(C.Register name) = show name ++ ": " ++ (show . C.getRegisterValue reg $ regs)

showDuo :: (Show a) => D.ExecutionContext a -> String
showDuo (D.ExecutionContext (n1, ec1) (n2, ec2)) = unlines $ zipWith (++) s1'' s2 where
  s1 = lines . showSolo $ ec1
  s2 = lines . showSolo $ ec2
  l = max (length s1) (length s2)
  s1' = s1 ++ (replicate d1 "")
    where d1 = l - (length s1)
  s2' = s2 ++ (replicate d2 "")
    where d2 = l - (length s2)
  s1'' = map pad s1'
  pad n = n ++ (replicate (30 - (length n)) ' ')