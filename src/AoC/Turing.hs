module AoC.Turing (makeTuringMachine, runTuringMachine, turingChecksum, parseTuringSetup) where

import qualified AoC.Turing.Parse as P
import qualified AoC.Turing.Core as C

parseTuringSetup :: String -> Either String C.TuringSetup
parseTuringSetup = P.parseTuringSetup

makeTuringMachine :: C.TuringSetup -> C.TuringMachine
makeTuringMachine = C.makeMachine

runTuringMachine :: C.TuringMachine -> C.TuringMachine
runTuringMachine = C.runTuring

turingChecksum :: C.TuringMachine -> Int
turingChecksum = C.turingChecksum
