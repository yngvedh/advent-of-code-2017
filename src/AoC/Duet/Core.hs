module AoC.Duet.Core (
  Instruction(..), Offset(..),
  Register(..), Value(..), Registers(..), Cpu(..),
  getRegisterValue, setRegister, updateRegister,
  setOutput, getOutput, getValue) where

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

data Cpu = Cpu Registers Int
  deriving (Eq, Show)

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

getValue :: Value -> Registers -> Int
getValue (LiteralValue v) _ = v
getValue (RegisterValue name) rs = getRegisterValue name rs 

getRegisterValue name (Registers rs) =
  case lookup name rs of
    Just v -> v
    Nothing -> 0
