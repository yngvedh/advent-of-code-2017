module AoC.Tubes.Core (
  Tubes, Tube(..),
  makeTubes, makeTubesAt,
  runAndRecordLetters, runAndRecordSteps) where

import Data.List (find)
import Data.Maybe (fromMaybe)
    
import qualified AoC.Focus.ListMatrix as F

data Tube = Vertical | Horizontal | Cross | Letter Char | Space
  deriving (Show, Eq)

newtype Tubes = Tubes (F.ListMatrixFocus Tube)
  deriving (Show, Eq)

makeTubes :: [[Tube]] -> Tubes
makeTubes = moveToStart . Tubes . F.makeFocus where
  moveToStart ts = fromMaybe ts . find atVertical . iterate moveEast $ ts
  atVertical = ((==) Vertical . get)

makeTubesAt :: Int -> Int -> [[Tube]] -> Tubes
makeTubesAt x y = Tubes . F.makeFocusAt x y

moveWest, moveSouth, moveNorth, moveEast :: Tubes -> Tubes
moveWest = withFocus F.moveLeft
moveEast = withFocus F.moveRight
moveNorth = withFocus F.moveUp
moveSouth = withFocus F.moveDown

canMoveWest, canMoveSouth, canMoveNorth, canMoveEast :: Tubes -> Bool
canMoveWest = not . F.isLeftMost . getFocus
canMoveEast = not . F.isRightMost . getFocus
canMoveNorth = not . F.isTopMost . getFocus
canMoveSouth = not . F.isBottomMost . getFocus

withFocus :: (F.ListMatrixFocus Tube -> F.ListMatrixFocus Tube) -> Tubes -> Tubes
withFocus f (Tubes ls) = Tubes $ f ls

getFocus :: Tubes -> F.ListMatrixFocus Tube
getFocus (Tubes f) = f

get :: Tubes -> Tube
get = F.get . getFocus

data Direction = North | South | East | West
  deriving (Eq)

move :: Direction -> Tubes -> Tubes
move South = moveSouth
move North = moveNorth
move West = moveWest
move East = moveEast

canMove :: Direction -> Tubes -> Bool
canMove South = canMoveSouth
canMove North = canMoveNorth
canMove East = canMoveEast
canMove West = canMoveWest

alternativeDirs dir | dir `elem` [East, West] = [North, South]
alternativeDirs _ = [East, West]

runAndRecordLetters :: Tubes -> String
runAndRecordLetters ts = recordedLetters $ run' South ts emptyRunResult
runAndRecordSteps :: Tubes -> Int
runAndRecordSteps ts = recordedSteps $ run' South ts emptyRunResult

data RunResult = RunResult String Int

emptyRunResult :: RunResult
emptyRunResult = RunResult "" 0

recordedLetters (RunResult letters _) = reverse letters
recordedSteps (RunResult _ steps) = steps

recordLetter c (RunResult letters steps) = RunResult (c:letters) steps
recordStep (RunResult letters steps) = RunResult letters (steps+1)


run' :: Direction -> Tubes -> RunResult -> RunResult
run' dir ts visited = if canMove dir ts && get ts' /= Space
  then run' dir ts' $ recordStep visited'
  else case dir' of
    Just d -> run' d ts visited'
    Nothing -> recordStep visited'
  where
    ts' = move dir ts
    dir' = find goesSomewhere . alternativeDirs $ dir
    goesSomewhere d = (get . move d $ ts) /= Space
    visited' = case get ts of
      Letter l -> recordLetter l visited
      _ -> visited