module AoC.Duet.Core (
  Instruction(..), Offset(..),
  Register(..), Value(..), Registers(..), Cpu(..), ExecutionContext(..),
  stepOnce, emptyExecutionContext) where

import AoC.Focus.List

import Data.List (nub, sort)

newtype Register = Register String
  deriving (Eq, Show, Ord)

newtype Offset = Offset Int
  deriving (Eq, Show)

data Value = LiteralValue Int | RegisterValue Register
  deriving (Eq, Show)

data Instruction =
  Snd Value |
  Set Register Value |
  Add Register Value |
  Mul Register Value |
  Mod Register Value |
  Rcv Register |
  Jgz Register Value
  deriving (Eq, Show)

newtype Registers = Registers [(Register, Int)]

instance Eq Registers where
  (/=) a b = if namesA /= namesB
    then False
    else valuesA /= valuesB
    where
      valuesA = map (flip getRegisterValue a) namesA
      valuesB = map (flip getRegisterValue b) namesB
      namesA = registerNames a
      namesB = registerNames b

instance Show Registers where
  show rs = concat . map show $ zip ns vs where
    ns = registerNames rs
    vs = map (flip getRegisterValue rs) ns

registerNames :: Registers -> [Register]
registerNames = nub . sort . map fst . regs where
  regs (Registers rs) = rs

data Cpu = Cpu Registers Int
  deriving (Eq, Show)

data ExecutionContext = ExecutionContext Cpu (ListFocus Instruction)
  deriving (Eq, Show)

emptyExecutionContext = ExecutionContext (Cpu (Registers []) 0) . makeFocus

withCpu :: (Cpu -> Cpu) -> ExecutionContext -> ExecutionContext
withCpu f (ExecutionContext c instr) = ExecutionContext (f c) instr

setOutput :: Int -> Cpu -> Cpu
setOutput hz (Cpu rs _) = Cpu rs hz

getOutput :: Cpu -> Int
getOutput (Cpu _ hz) = hz

setRegister :: Register -> Int -> Cpu -> Cpu
setRegister reg v (Cpu (Registers rs) hz) = Cpu (Registers rs') hz where
  rs' = (reg,v):rs

updateRegister :: Register -> (Int -> Int) -> Cpu -> Cpu
updateRegister reg f c = setRegister reg v' c where
  v' = f $ getValue (RegisterValue reg) rs
  (Cpu rs _) = c

stepOnce :: ExecutionContext -> ExecutionContext
stepOnce ec@(ExecutionContext _ is) =
  executeInstruction instr ec where
    instr = get is

registers :: ExecutionContext -> Registers
registers (ExecutionContext (Cpu rs _) _) = rs

jumpNext :: ExecutionContext -> ExecutionContext
jumpNext = withInstructions moveRight

executeInstruction :: Instruction -> ExecutionContext -> ExecutionContext
executeInstruction (Snd hzVal) ec = jumpNext . withCpu (setOutput hz) $ ec where
  hz = getValue hzVal $ registers ec
executeInstruction (Set reg val) ec = jumpNext . withCpu (setRegister reg v) $ ec where
  v = getValue val $ registers ec
executeInstruction (Add reg val) ec = jumpNext . executeArithmetic (+) reg val $ ec
executeInstruction (Mul reg val) ec = jumpNext . executeArithmetic (*) reg val $ ec
executeInstruction (Mod reg val) ec = jumpNext . executeArithmetic mod reg val $ ec
executeInstruction (Rcv reg) ec@(ExecutionContext c _) =
  if getRegisterValue reg (registers ec) /= 0
  then jumpNext . withCpu (setRegister reg $ getOutput c) $ ec
  else jumpNext ec
executeInstruction (Jgz reg val) ec =
  if getRegisterValue reg (registers ec) /= 0 then
    let offset = Offset . getValue val . registers $ ec in
      jump offset ec
  else jumpNext ec
      

jump :: Offset -> ExecutionContext -> ExecutionContext
jump (Offset 0) ec = ec
jump (Offset o) ec = if o < 0
  then jump (Offset $ o+1) $ withInstructions moveRight ec
  else jump (Offset $ o-1) $ withInstructions moveLeft ec

withInstructions :: (ListFocus Instruction -> ListFocus Instruction) -> ExecutionContext -> ExecutionContext
withInstructions f (ExecutionContext rs is) = ExecutionContext rs (f is)

executeArithmetic :: (Int -> Int -> Int) -> Register -> Value -> ExecutionContext -> ExecutionContext
executeArithmetic op reg val ec = withCpu (updateRegister reg $ flip op v) ec where
  v = getValue val $ registers ec


getValue :: Value -> Registers -> Int
getValue (LiteralValue v) _ = v
getValue (RegisterValue name) rs = getRegisterValue name rs 

getRegisterValue name (Registers rs) =
  case lookup name rs of
    Just v -> v
    Nothing -> 0
