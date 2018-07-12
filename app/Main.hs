module Main where

import AoC.Misc
import AoC.CircularMemory
import AoC.SpreadSheet
import AoC.PassPhrase
import AoC.CPU
import AoC.BlockMemory
import AoC.RecursiveTower
import AoC.RegisterInstructions
import AoC.Stream
import AoC.KnotHash
import AoC.HexGrid
import AoC.Plumbing
import AoC.Firewall
import AoC.Disk
import AoC.Generator
import AoC.PermProm
import AoC.SpinLock
import AoC.Duet

import Data.Char (digitToInt)
import Data.List (find)
import System.Environment (getArgs)
import Data.Maybe (fromJust)

main :: IO ()
main = do
  args <- getArgs
  let day = (read $ head args) :: Int
  input <- readFile $ "input/" ++ (show day) ++ ".txt"
  solveDay day input

solveDay :: Int -> String -> IO ()
solveDay day =
  case day of
    1 -> day1
    2 -> day2
    3 -> day3
    4 -> day4
    5 -> day5
    6 -> day6
    7 -> day7
    8 -> day8
    9 -> day9
    10 -> day10
    11 -> day11
    12 -> day12
    13 -> day13
    14 -> day14
    15 -> day15
    16 -> day16
    17 -> day17
    18 -> day18
    _ -> unsolvedDay


unsolvedDay :: String -> IO ()
unsolvedDay _ = putStrLn "Day not solved yet."

day1 input = do
  let digits = map digitToInt $ (head . lines) input
  print $ AoC.Misc.sumConsecutiveEquals digits
  print $ AoC.Misc.sumOppositeEquals digits


day2 :: String -> IO ()
day2 input = do
  let sheet = map (map (read :: String -> Int)) $ map words $ lines input
  print $ AoC.SpreadSheet.checksum sheet
  print $ AoC.SpreadSheet.sumOfDivisions sheet

day3 input = do
  let adr = (read input) :: Int
  print $ AoC.CircularMemory.distanceToAddress adr
  print $ fromJust $ find (> adr) AoC.CircularMemory.initialValues

day4 :: String -> IO ()
day4 input = do
  let phrases = lines input
  let n = length $ filter (== True) $ map AoC.PassPhrase.isValid phrases
  let n' = length $ filter (== True) $ map AoC.PassPhrase.isValidNoAnagrams phrases
  print n
  print n'

countJumps :: (Jumps -> Jumps) -> Jumps -> Int
countJumps step' js =
  if isCpuOutside js then 0
  else 1 + (countJumps step' $ step' js)

day5 :: String -> IO ()
day5 input = do
  let jumps = makeInitialJumps $ map (read :: String -> Int) $ lines input
  let n = countJumps step jumps
  let n' = countJumps weirdStep jumps
  print n
  print n'

day6 input = do
  let memory = AoC.BlockMemory.makeMemory $ (map read $ words input :: [Int])
  let cycle = AoC.BlockMemory.balance memory
  print . length $ cycle
  print . length $ dropWhile ((/=) $ AoC.BlockMemory.balanceStep $ last cycle) cycle

day7 input =
  case AoC.RecursiveTower.parseTower input of
    Left error -> putStrLn $ "Parse error: " ++ error
    Right tower -> do
      putStrLn $ towerName tower
      case findUnbalanced tower of
        Just t -> putStrLn $ unbalancedReport t
        Nothing -> putStrLn "Tower is balanced."

unbalancedReport tower = "Found unbalanced: '" ++ (towerName tower) ++ "' New weight: " ++ (show $ balancingWeight tower)

day8 = dayWithParserAndSolver parseInstructions day8Solver

day8Solver instructions = do
  let (result, largest) = executeInstructionsAndTraceLargestValue makeRegisters instructions
  print . largestValue $ result
  print largest

dayWithParserAndSolver parser solver input =
  case parser input of
    Left error -> putStrLn $ "Parser error: " ++ error
    Right parsed -> solver parsed

day9 = dayWithParserAndSolver parseStream day9Solver

day9Solver root = do
  let score = scoreGroup root
  let n = countGarbage root
  putStrLn $ "Score: " ++ show score
  putStrLn $ "#Garbage: " ++ show n

day10 = dayWithParserAndSolver parseKnotInput day10Solver

day10Solver (asciiLengths, byteLengths) = do
  let hash = computeKnotHash asciiLengths
  let fullHash = computeFullKnotHash byteLengths
  putStrLn $ "Hash: " ++ show hash
  putStrLn $ "Real hash:" ++ showFullKnotHash fullHash

day11 = dayWithParserAndSolver parseHexPath day11Solver

day11Solver path = do
  let dist = hexManhattanDist . hexEndPos $ path
  let farthestDist = hexFarthestDist path
  putStrLn $ "Distance: " ++ show dist
  putStrLn $ "Farthest distance: " ++ show farthestDist

day12 = dayWithParserAndSolver parsePlumbing day12Solver

day12Solver village = do
  let group = getGroup village 0
  let groups = getGroups village
  putStrLn $ "Size of group which program 0 is part of: " ++ (show . length $ group)
  putStrLn $ "Number of groups: " ++ (show . length $ groups)

day13 = dayWithParserAndSolver parseFirewall day13Solver

day13Solver firewall = do
  let score = scoreFirewall firewall
  let delay = undetectableDelay firewall
  putStrLn $ "Score: " ++ (show score)
  putStrLn $ "Minimum delay to pass undetected: " ++ (show delay)

day14 = dayWithParserAndSolver (Right . diskFromKey) day14Solver

day14Solver disk = do
  let t = showDisk disk
  let used = length . filter ((==) '#') $ t
  let regions = countDiskRegions disk
  putStrLn $ "Used squares: " ++ (show used)
  putStrLn $ "Number of regions: " ++ (show regions)

day15 = dayWithParserAndSolver parseGenerators day15Solver
day15Solver ((s1,d1,m1), (s2,d2,m2)) = do
  let g1 = makeSimpleGenerator s1 m1
  let g2 = makeSimpleGenerator s2 m2
  let f1 = makeFilterGenerator s1 d1 m1
  let f2 = makeFilterGenerator s2 d2 m2
  let count = judgeGenerators 40000000 g1 g2
  let count2 = judgeGenerators 5000000 f1 f2
  putStrLn $ "Number of matches (simple): " ++ (show count)
  putStrLn $ "Number of matches (filter): " ++ (show count2)

day16 = dayWithParserAndSolver parseDanceMoves day16Solver
day16Solver moves = do
  let many = 1000000000
  let dancers = map makeDancer ['a'..'p']
  let result = map dancerName . dance moves $ dancers
  let result' = map dancerName . repeatDance many moves $ dancers
  putStrLn $ "Dancers ended up at: " ++ result
  putStrLn $ "Dancers ended up at (after " ++ (show many) ++ " times): " ++ result'

day17 = dayWithParserAndSolver parseSkipDistance day17Solver
day17Solver dist = do
  let i = currentValue . spinRight 1 . spinN 2017 dist 1 $ emptySpinLock
  let i' = simulateValueAtPos1 dist 50000000
  putStrLn $ "The value after 2017, is: " ++ (show i)
  putStrLn $ "The value of pos 1 after 50000000 insertions, is: " ++ (show i')

day18 = dayWithParserAndSolver parseDuetProgram day18Solver

day18Solver is = do
  let ec = makeDuetExecutionContext is
  let ec' = runSoloFirstRcv ec
  let hz = duetOutputFrequency ec'
  putStrLn $ "The output frequency when first read is: " ++ (show hz)