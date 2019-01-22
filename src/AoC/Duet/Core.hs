module AoC.Duet.Core (
  Instruction(..), Offset(..), Program(..),
  Register(..), Value(..), Registers(..), Cpu(..),
  getRegisterValue, setRegister, withRegister, getRegister, registerNames,
  getValue, jumpNext,
  executeCpuInstruction, nextInstruction, makeProgram,
  makeRegisters, makeRegistersFromList,
  instructions, isFaulted) where

import Data.List (nub, sort)
import qualified Data.Map.Strict as M
import qualified Data.Text as T

import qualified AoC.Focus.List as F;

newtype Register = Register T.Text
  deriving (Eq, Show, Ord)

newtype Offset = Offset Int
  deriving (Eq, Show)

data Value = LiteralValue Int | RegisterValue Register
  deriving (Eq, Show)

data Instruction =
  Snd Value |
  Set Register Value |
  Add Register Value |
  Sub Register Value |
  Mul Register Value |
  Mod Register Value |
  Rcv Register |
  Jgz Value Value |
  Jnz Value Value
  deriving (Eq, Show)

data Program = Program (F.ListFocus Instruction) | OutOfBounds
  deriving (Eq, Show)

instructions (Program f) = f

makeProgram :: [Instruction] -> Program
makeProgram = Program . F.makeFocus

moveRight, moveLeft :: Program -> Program
moveRight = mapProgramIf F.moveRight (not . F.isRightMost)
moveLeft = mapProgramIf F.moveLeft (not . F.isLeftMost)

mapProgramIf ::
  (F.ListFocus Instruction -> F.ListFocus Instruction) ->
  (F.ListFocus Instruction -> Bool) ->
  Program ->
  Program
mapProgramIf f pred p =
  if pred focus
    then Program . f $ focus
    else OutOfBounds
  where
    focus = instructions p

isFaulted :: Cpu -> Bool
isFaulted = (==) OutOfBounds . program

nextInstruction :: Cpu -> Instruction
nextInstruction = F.get . instructions . program

jump :: Offset -> Cpu -> Cpu
jump (Offset 0) cpu = cpu
jump (Offset o) cpu = if o < 0
  then jump (Offset $ o+1) (jumpPrev cpu)
  else jump (Offset $ o-1) (jumpNext cpu)

jumpNext, jumpPrev :: Cpu -> Cpu
jumpNext = withProgram moveRight
jumpPrev = withProgram moveLeft

withProgram :: (Program -> Program) -> Cpu -> Cpu
withProgram f (Cpu rs is) = Cpu rs (f is)

program :: Cpu -> Program
program (Cpu _ p) = p

programAtStart, programAtEnd :: Cpu -> Bool
programAtStart = F.isLeftMost . instructions . program
programAtEnd = F.isRightMost . instructions . program


newtype Registers = Registers (M.Map Register Int)
  deriving (Eq)

makeRegisters :: Registers
makeRegisters = Registers M.empty
makeRegistersFromList :: [(Register, Int)] -> Registers
makeRegistersFromList = Registers . M.fromList

instance Show Registers where
  show rs = "[" ++ ts ++ "]"  where
    ts = concatMap show $ zip ns vs
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
executeCpuInstruction (Sub reg val) cpu = jumpNext . executeArithmetic (-) reg val $ cpu
executeCpuInstruction (Mul reg val) cpu = jumpNext . executeArithmetic (*) reg val $ cpu
executeCpuInstruction (Mod reg val) cpu = jumpNext . executeArithmetic mod reg val $ cpu
executeCpuInstruction i@(Jgz val offsetVal) cpu = conditionalJump ((<) 0) val offsetVal cpu
executeCpuInstruction i@(Jnz val offsetVal) cpu = conditionalJump ((/=) 0) val offsetVal cpu

conditionalJump pred val offsetVal cpu@(Cpu rs p) =
  if pred $ getValue val cpu then
    let offset = Offset . getValue offsetVal $ cpu in
      jump offset cpu
  else jumpNext cpu

executeArithmetic :: (Int -> Int -> Int) -> Register -> Value -> Cpu -> Cpu
executeArithmetic op reg val cpu = withRegister reg (flip op $ getValue val cpu) cpu where
