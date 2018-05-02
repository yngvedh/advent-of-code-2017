module AoC.RegisterInstructions.Core (
  Instruction(..), Comparison(..), Name(..), Registers(..),
  makeRegisters,
  registerValue, updateRegister,
  executeInstructions, executeInitialInstructions,
  largestValue, executeInstructionsAndTraceLargestValue) where

import Data.List (delete)

data Name = Name String
  deriving (Show, Eq)

data Comparison = Less | Greater | LessEq | GreaterEq | Equal | Inequal
  deriving (Show, Eq)

data Instruction = Instruction Name Int Name Comparison Int
  deriving (Show, Eq)

instructionRegister (Instruction name _ _ _ _) = name
instructionOffset (Instruction _ offset _ _ _) = offset
instructionCondition :: Instruction -> (Int -> Bool)
instructionCondition (Instruction _ _ _ comp n) = (flip $ instructionComparator comp) n
instructionComparator Less = (<)
instructionComparator Greater = (>)
instructionComparator LessEq = (<=)
instructionComparator GreaterEq = (>=)
instructionComparator Equal = (==)
instructionComparator Inequal = (/=)
instructionComparisonRegister (Instruction _ _ name _ _) = name

data Registers = Registers [(Name, Int)]
  deriving (Show, Eq)

makeRegisters = Registers []

registerValue :: Name -> Registers -> Int
registerValue name (Registers regs) = case lookup name regs of
  Just v -> v
  Nothing -> 0

updateRegister :: Name -> Int -> Registers -> Registers
updateRegister name offset (Registers regs) = case lookup name regs of
  Just v -> Registers $ (name, v + offset):(delete (name, v) regs)
  Nothing -> Registers $ (name, offset):regs

executeInstruction :: Instruction -> Registers -> Registers
executeInstruction instr regs = let
  cond = instructionCondition instr
  val' = registerValue name' regs
  name = instructionRegister instr
  name' = instructionComparisonRegister instr
  offset = instructionOffset instr in
    if cond val'
    then updateRegister name offset regs
    else regs

executeInstructions :: Registers -> [Instruction] -> Registers
executeInstructions = foldl $ flip executeInstruction

executeInitialInstructions = executeInstructions makeRegisters

largestValue :: Registers -> Int
largestValue (Registers []) = 0
largestValue (Registers regs) = maximum $ map (\(_,v) -> v) regs

executeInstructionsAndTraceLargestValue :: Registers -> [Instruction] -> (Registers, Int)
executeInstructionsAndTraceLargestValue regs = foldl executeNext (regs,0) where
  executeNext (regs,l) instr = (regs', max l l') where 
    regs' = executeInstruction instr regs
    l' = registerValue (instructionRegister instr) regs'