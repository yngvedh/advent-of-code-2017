module AoC.RegisterInstructions (
  parseInstructions, executeInstructions, makeRegisters,
  largestValue, executeInstructionsAndTraceLargestValue) where

import qualified AoC.RegisterInstructions.Parse as P
import qualified AoC.RegisterInstructions.Core as C

parseInstructions = P.parseInstructions
makeRegisters = C.makeRegisters
executeInstructions = C.executeInstructions
largestValue = C.largestValue
executeInstructionsAndTraceLargestValue = C.executeInstructionsAndTraceLargestValue
