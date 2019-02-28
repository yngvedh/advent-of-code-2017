module AoC.Turing.Core (
  TuringStates, makeStates,
  TuringSetup, makeSetup,
  TuringMachine, makeMachine,
  TuringState, makeState,
  TuringAction, makeAction,
  TapeValue (..),
  Movement (..),
  Name (..),
  currentState, currentValue, tapeTailLeft, tapeTailRight,
  runTuring,
  turingChecksum) where

import qualified Data.Map.Strict as M
import Data.Maybe (fromJust)

import qualified AoC.Focus.List as F

newtype Name = Name String
    deriving (Eq, Show, Ord)

data TapeValue = Set | Reset
  deriving (Eq, Show)
data Movement = MoveLeft | MoveRight
  deriving (Eq, Show)

data TuringAction = Action { actionNewValue :: TapeValue, actionMovement :: Movement, nextState :: Name }
  deriving (Eq, Show)

makeAction :: TapeValue -> Movement -> Name -> TuringAction
makeAction = Action

data TuringState = State { resetAction :: TuringAction, setAction :: TuringAction }
  deriving (Eq, Show)

data TuringTape = Tape (F.ListFocus TapeValue)
  deriving (Eq, Show)

getValue :: TuringTape -> TapeValue
getValue (Tape f) = F.get f

setValue :: TapeValue -> TuringTape -> TuringTape
setValue t (Tape f) = Tape . F.set t $ f

moveTapeLeft, moveTapeRight :: TuringTape -> TuringTape
moveTapeLeft (Tape f) = Tape . F.moveLeft $ f' where
  f' = if F.isLeftMost f then F.appendLeft Reset f else f
moveTapeRight (Tape f) = Tape . F.moveRight $ f' where
  f' = if F.isRightMost f then F.appendRight Reset f else f

makeTape :: TuringTape
makeTape = Tape . F.makeFocus $ [Reset]

makeState :: TuringAction -> TuringAction -> TuringState
makeState = State

newtype TuringStates = States (M.Map Name TuringState)
  deriving (Eq, Show)

getState :: Name -> TuringStates -> TuringState
getState n (States m) = fromJust . M.lookup n $ m

makeStates :: [(Name, TuringState)] -> TuringStates
makeStates = States . M.fromList

data TuringSetup = Setup { initialState :: Name, setupSteps :: Int,  setupStates :: TuringStates }
  deriving (Eq, Show)

makeSetup :: Name -> Int -> TuringStates -> TuringSetup
makeSetup = Setup

data TuringMachine = Machine {
   machineTape :: TuringTape,
   machineStates :: TuringStates,
   currentState :: Name,
   machineSteps :: Int }
  deriving (Eq, Show)

makeMachine :: TuringSetup -> TuringMachine
makeMachine (Setup state steps states) = Machine makeTape states state steps

currentValue :: TuringMachine -> TapeValue
currentValue = getValue . machineTape

tapeTailLeft, tapeTailRight :: TuringMachine -> [TapeValue]
tapeTailLeft tm = let (Tape f) = machineTape tm in F.prefix f
tapeTailRight tm = let (Tape f) = machineTape tm in F.postfix f

withTape :: (TuringTape -> TuringTape) -> TuringMachine -> TuringMachine
withTape f tm = tm { machineTape = f . machineTape $ tm }

moveLeft, moveRight :: TuringMachine -> TuringMachine
moveLeft = withTape moveTapeLeft
moveRight = withTape moveTapeRight

stepMachine :: TuringMachine -> TuringMachine
stepMachine tm@(Machine tape states state steps) =
  Machine tape' states state' (steps-1) where
  cur = getValue tape
  curState = getState state states
  action = case cur of
    Reset -> resetAction curState
    Set   -> setAction curState
  tape' = move . setValue (actionNewValue action) $ tape
  move = case actionMovement action of
    MoveLeft  -> moveTapeLeft
    MoveRight -> moveTapeRight
  state' = nextState action

runTuring :: TuringMachine -> TuringMachine
runTuring tm@(Machine _ _ _ 0) = tm
runTuring tm = until ((==) 0 . machineSteps) stepMachine tm

turingChecksum :: TuringMachine -> Int
turingChecksum tm = let (Tape f) = machineTape tm in length . filter ((==) Set) . F.unfocus $ f

