module AoC.Turing.Parse (parseTuringSetup) where

import AoC.Misc (mapLeft)
import AoC.ParsePrimitives

import AoC.Turing.Core

import Text.Parsec
import Text.Parsec.String

parseTuringSetup :: String -> Either String TuringSetup
parseTuringSetup = mapLeft show . parse turingSetup ""

turingSetup :: Parser TuringSetup
turingSetup = do
  state0 <- initialState
  numSteps <- steps
  ss <- states
  return $ makeSetup state0 numSteps ss

initialState :: Parser Name
initialState = do
  string "Begin in state "
  n <- name
  string ".\n"
  return n

name :: Parser Name
name = Name <$> (many1 letter)

steps :: Parser Int
steps = do
  string "Perform a diagnostic checksum after "
  n <- integer
  string " steps.\n"
  return n

states :: Parser TuringStates
states = makeStates <$> (many1 state)

state :: Parser (Name, TuringState)
state = do
  many $ char '\n'
  n <- stateName
  resetAction <- action '0'
  setAction   <- action '1'
  return (n, makeState resetAction setAction)

stateName :: Parser Name
stateName = do
  string "In state "
  n <- name
  string ":\n"
  return n

action :: Char -> Parser TuringAction
action c = do
  string $ "  If the current value is " ++ (c:":\n")
  val <- writeValue
  move <- movement
  next <- nextState
  return $ makeAction val move next

writeValue :: Parser TapeValue
writeValue = do
  string "    - Write the value "
  n <- tapeValue
  string ".\n"
  return n

movement :: Parser Movement
movement = do
  string "    - Move one slot to the "
  m <- direction
  string ".\n"
  return m

direction :: Parser Movement
direction = MoveLeft  <$ string "left"
        <|> MoveRight <$ string "right"

nextState :: Parser Name
nextState = do
  string "    - Continue with state "
  n <- name
  string ".\n"
  return n

tapeValue :: Parser TapeValue
tapeValue = Set   <$ char '1'
        <|> Reset <$ char '0'