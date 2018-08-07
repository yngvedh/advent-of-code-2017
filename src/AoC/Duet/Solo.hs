module AoC.Duet.Solo (
  ExecutionContext(..),
  stepOnce, runSoloFirstRcv, emptyExecutionContext) where

import AoC.Focus.List
import AoC.Duet.Core

data ExecutionContext = ExecutionContext Cpu (ListFocus Instruction)
  deriving (Eq)

instance Show ExecutionContext where
  show (ExecutionContext cpu is) = "ËxecutionContext:\n" ++ show cpu ++ "\n" ++ show' is where
    show' :: ListFocus Instruction -> String
    show' l = unlines $ (map ((++) "  ") . map show . prefix $ l) ++ ["> " ++ show (get l)] ++ (map ((++) "  ") . map show . postfix $ l)

emptyExecutionContext :: [Instruction] -> ExecutionContext
emptyExecutionContext = ExecutionContext (Cpu (Registers []) 0) . makeFocus

withCpu :: (Cpu -> Cpu) -> ExecutionContext -> ExecutionContext
withCpu f (ExecutionContext c instr) = ExecutionContext (f c) instr

stepOnce :: ExecutionContext -> ExecutionContext
stepOnce ec =
  executeInstruction instr ec where
    instr = nextInstruction ec

runSoloFirstRcv :: ExecutionContext -> ExecutionContext
runSoloFirstRcv ec = if isRcv . nextInstruction $ ec then ec' else runSoloFirstRcv ec' where
  ec' = stepOnce ec
  isRcv (Rcv reg) = 0 /= (getRegister reg . cpu $ ec)
  isRcv _ = False

nextInstruction :: ExecutionContext -> Instruction
nextInstruction (ExecutionContext _ is) = get is

cpu :: ExecutionContext -> Cpu
cpu (ExecutionContext cpu _) = cpu

jumpNext :: ExecutionContext -> ExecutionContext
jumpNext = withInstructions moveRight

executeInstruction :: Instruction -> ExecutionContext -> ExecutionContext
executeInstruction (Snd hzVal) ec = jumpNext . withCpu (setOutput hz) $ ec where
  hz = getValue hzVal $ cpu ec
executeInstruction (Rcv reg) ec@(ExecutionContext c _) =
  if getRegister reg (cpu ec) /= 0
  then jumpNext . withCpu (setRegister reg $ getOutput c) $ ec
  else jumpNext ec
executeInstruction (Jgz reg val) ec =
  if getRegister reg (cpu ec) > 0 then
    let offset = Offset . getValue val . cpu $ ec in
      jump offset ec
  else jumpNext ec
executeInstruction i ec = jumpNext . withCpu (executeCpuInstruction i) $ ec

jump :: Offset -> ExecutionContext -> ExecutionContext
jump (Offset 0) ec = ec
jump (Offset o) ec = if o < 0
  then jump (Offset $ o+1) $ withInstructions moveLeft ec
  else jump (Offset $ o-1) $ withInstructions moveRight ec

withInstructions :: (ListFocus Instruction -> ListFocus Instruction) -> ExecutionContext -> ExecutionContext
withInstructions f (ExecutionContext rs is) = ExecutionContext rs (f is)

executeArithmetic :: (Int -> Int -> Int) -> Register -> Value -> ExecutionContext -> ExecutionContext
executeArithmetic op reg val ec@(ExecutionContext cpu _) = flip withCpu ec $ withRegister reg (flip op v)  where
  v = getValue val cpu
