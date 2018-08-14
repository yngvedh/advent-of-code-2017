module AoC.Duet.Core (
  Instruction(..), Offset(..),
  Register(..), Value(..), Registers(..), Cpu(..),
  getRegisterValue, setRegister, withRegister, getRegister,
  getValue,
  executeCpuInstruction) where

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
  (/=) a b = (namesA == namesB) && valuesA /= valuesB
    where
      valuesA = map (flip getRegisterValue a) namesA
      valuesB = map (flip getRegisterValue b) namesB
      namesA = registerNames a
      namesB = registerNames b

instance Show Registers where
  show rs = concatMap show $ zip ns vs where
    ns = registerNames rs
    vs = map (flip getRegisterValue rs) ns

registerNames :: Registers -> [Register]
registerNames = nub . sort . map fst . regs where
  regs (Registers rs) = rs

data Cpu = Cpu Registers
  deriving (Eq, Show)

withRegister :: Register -> (Int -> Int) -> Cpu -> Cpu
withRegister reg f cpu = (setRegister reg . f . getRegister reg $ cpu) cpu

setRegister :: Register -> Int -> Cpu -> Cpu
setRegister reg v (Cpu (Registers rs)) = Cpu (Registers rs') where
  rs' = (reg,v):rs

getRegister :: Register -> Cpu -> Int
getRegister reg cpu@(Cpu rs) = getRegisterValue reg rs

getValue :: Value -> Cpu -> Int
getValue (LiteralValue v) _ = v
getValue (RegisterValue reg) cpu = getRegister reg cpu

getRegisterValue :: Register -> Registers -> Int
getRegisterValue name (Registers rs) =
  case lookup name rs of
    Just v -> v
    Nothing -> 0

executeCpuInstruction :: Instruction -> Cpu -> Cpu
executeCpuInstruction (Set reg val) cpu = setRegister reg (getValue val cpu) cpu where
executeCpuInstruction (Add reg val) cpu = executeArithmetic (+) reg val cpu
executeCpuInstruction (Mul reg val) cpu = executeArithmetic (*) reg val cpu
executeCpuInstruction (Mod reg val) cpu =  executeArithmetic mod reg val cpu

executeArithmetic :: (Int -> Int -> Int) -> Register -> Value -> Cpu -> Cpu
executeArithmetic op reg val cpu = withRegister reg (flip op $ getValue val cpu) cpu where
