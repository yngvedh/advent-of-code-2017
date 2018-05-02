module AoC.RecursiveTower (Tower(..), parseTower, towerName, findUnbalanced, balancingWeight) where

import Data.List (find, (\\), nub, groupBy, sortBy)
import Data.Maybe (listToMaybe, catMaybes)

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Prim

data Tower = Tower String Int [Tower]
  deriving (Show, Eq)

towerName (Tower name _ _) = name

towerWeight (Tower _ weight _) = weight

subTowers (Tower _ _ ts) = ts

recursiveWeight (Tower _ w cs) = w + w' where
  w' = sum $ map recursiveWeight cs

name = many letter

weight :: Parser Int
weight = do
  char '('
  s <- many1 digit
  char ')'
  return $ (read s :: Int)

edges :: Parser [String]
edges = do
  edgeIndicator <- optionMaybe $ string " -> "
  case edgeIndicator of
    Nothing -> return []
    Just ss -> many1 letter `sepBy1` string ", "

-- <name> (<weight>) [-> <name-1> [, <name-n]]
program :: Parser (String,Int,[String])
program = do
  n <- name
  char ' '
  w <- weight
  es <- edges
  return (n, w, es)

tower = do
  programs <- program `sepEndBy` string "\n"
  return $ constructTower programs

childNames (_,_,n) = n
programName (n,_,_) = n

constructTower :: [(String,Int,[String])] -> Tower
constructTower programs = constructTower' rootName where
  rootName = head $ (map programName programs) \\ allChildNames
  allChildNames = concat $ map childNames programs
  constructTower' name = Tower name' weight' $ map constructTower' childNames' where
    (Just (name',weight',childNames')) = find ((==) name . programName) programs

parseTower :: String -> Either String Tower
parseTower s = 
  case parse tower "" s of
    Left error -> Left $ show error
    Right t -> Right t

findUnbalanced :: Tower -> Maybe Tower
findUnbalanced (Tower _ _ []) = Nothing
findUnbalanced t@(Tower _ _ cs) =
  case listToMaybe . catMaybes $ map findUnbalanced cs of
    Nothing ->
      if isUnbalanced t
      then Just t
      else Nothing
    t' -> t'

isUnbalanced :: Tower -> Bool
isUnbalanced (Tower _ _ cs) = distinctWeights > 1 where
  distinctWeights = length . nub $ map recursiveWeight cs

subweightSummary :: [Tower] -> [[Tower]]
subweightSummary = sortBy compareLength . groupBy compareWeights where
  compareWeights t t' = (recursiveWeight t) == (recursiveWeight t')
  compareLength ts ts' = compare (length ts) (length ts')

balancingWeight :: Tower -> Int
balancingWeight t = case subweightSummary $ subTowers t of
  [] -> -1
  [_] -> 0
  ([u]:(b:_):_) -> bw - (uw - uw') where
    bw = recursiveWeight b
    uw = recursiveWeight u
    uw' = towerWeight u
