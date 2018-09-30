module AoC.Duet.Core (
  Instruction(..), Offset(..), Program(..),
  Register(..), Value(..), Registers(..), Cpu(..),
  getRegisterValue, setRegister, withRegister, getRegister, registerNames,
  getValue, jumpNext,
  executeCpuInstruction, nextInstruction, makeProgram,
  makeRegisters, makeRegistersFromList,
  instructions) where

import Data.List (nub, sort)
import qualified Data.Map.Strict as M

import qualified AoC.Focus.List as F;

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
  Jgz Value Value
  deriving (Eq, Show)

data Program = Program (F.ListFocus Instruction)
  deriving (Eq, Show)

instructions (Program f) = f

makeProgram :: [Instruction] -> Program
makeProgram = Program . F.makeFocus

moveRight, moveLeft :: Program -> Program
moveRight = Program . F.moveRight . verify (not . F.isRightMost) . instructions
moveLeft = Program . F.moveLeft . verify (not . F.isLeftMost) . instructions

verify :: (a -> Bool) -> a -> a
verify pred a = if pred a then a else error "IS is moved out of bouds"

nextInstruction :: Cpu -> Instruction
nextInstruction = F.get . instructions . program

jump :: Offset -> Cpu -> Cpu
jump (Offset 0) ec = ec
jump (Offset o) ec = if o < 0
  then jump (Offset $ o+1) $ withProgram moveLeft ec
  else jump (Offset $ o-1) $ withProgram moveRight ec

withProgram :: (Program -> Program) -> Cpu -> Cpu
withProgram f (Cpu rs is) = Cpu rs (f is)

program :: Cpu -> Program
program (Cpu _ p) = p

jumpNext :: Cpu -> Cpu
jumpNext = withProgram moveRight
  
newtype Registers = Registers (M.Map Register Int)

makeRegisters :: Registers
makeRegisters = Registers M.empty
makeRegistersFromList :: [(Register, Int)] -> Registers
makeRegistersFromList = Registers . M.fromList

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
registerNames (Registers rs) = M.keys rs

data Cpu = Cpu Registers Program
  deriving (Eq, Show)

withRegister :: Register -> (Int -> Int) -> Cpu -> Cpu
withRegister reg f cpu = (setRegister reg . f . getRegister reg $ cpu) cpu

setRegister :: Register -> Int -> Cpu -> Cpu
setRegister reg v (Cpu (Registers rs) p) = Cpu (Registers rs') p where
  rs' = M.insert reg v rs

getRegister :: Register -> Cpu -> Int
getRegister reg cpu@(Cpu rs _) = getRegisterValue reg rs

getValue :: Value -> Cpu -> Int
getValue (LiteralValue v) _ = v
getValue (RegisterValue reg) cpu = getRegister reg cpu

getRegisterValue :: Register -> Registers -> Int
getRegisterValue name (Registers rs) =
  case M.lookup name rs of
    Just v -> v
    Nothing -> 0

executeCpuInstruction :: Instruction -> Cpu -> Cpu
executeCpuInstruction (Set reg val) cpu = jumpNext . setRegister reg (getValue val cpu) $ cpu where
executeCpuInstruction (Add reg val) cpu = jumpNext . executeArithmetic (+) reg val $ cpu
executeCpuInstruction (Mul reg val) cpu = jumpNext . executeArithmetic (*) reg val $ cpu
executeCpuInstruction (Mod reg val) cpu = jumpNext . executeArithmetic mod reg val $ cpu
executeCpuInstruction (Jgz val offsetVal) cpu@(Cpu rs p) =
  if getValue val cpu > 0 then
    let offset = Offset . getValue offsetVal $ cpu in
      jump offset cpu
  else jumpNext cpu

executeArithmetic :: (Int -> Int -> Int) -> Register -> Value -> Cpu -> Cpu
executeArithmetic op reg val cpu = withRegister reg (flip op $ getValue val cpu) cpu where
