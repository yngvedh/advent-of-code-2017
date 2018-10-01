module AoC.Tubes (parseTubes, runAndRecordLetters, runAndRecordSteps) where

import qualified AoC.Tubes.Core as C
import qualified AoC.Tubes.Parse as P

parseTubes = P.parseTubes
runAndRecordLetters = C.runAndRecordLetters
runAndRecordSteps = C.runAndRecordSteps