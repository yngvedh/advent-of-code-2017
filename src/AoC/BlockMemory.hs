module AoC.BlockMemory (Memory, makeMemory, balanceStep, balance) where

import Data.List (unfoldr, findIndex)

data Memory = Memory [Int]
  deriving (Eq, Show)

data MemoryLens = MemoryLens [Int] Int [Int]

makeMemory :: [Int] -> Memory
makeMemory = Memory

asLens :: Memory -> MemoryLens
asLens (Memory (b:bs)) = MemoryLens [] b bs where

asMemory :: MemoryLens -> Memory
asMemory (MemoryLens front cur back) = Memory $ (reverse front) ++ (cur:back)

balance :: Memory -> [Memory]
balance  = map asMemory . balanceLens . asLens

balanceStep = asMemory . redistributeHighest . asLens

balanceLens :: MemoryLens -> [MemoryLens]
balanceLens m = (m:(unfoldr next [m])) where
  next ms = let
    m' = redistributeHighest $ head ms
    memEq a b = (asMemory a) == (asMemory b) in
      if any (memEq m') ms then Nothing else Just (m', m':ms)

redistributeHighest :: MemoryLens -> MemoryLens
redistributeHighest = redistributeCurrent . moveToHighest

moveToHighest :: MemoryLens -> MemoryLens
moveToHighest = moveToHighest' . asMemory where
  moveToHighest' (Memory bs) = (MemoryLens pre cur post) where
    (Just i) = findIndex ((==) $ maximum bs) bs
    pre = reverse $ take i bs
    cur = head $ drop i bs
    post = tail $ drop i bs

largestBlockSize (MemoryLens [] cur []) = cur
largestBlockSize (MemoryLens pre cur []) = max (maximum pre) cur
largestBlockSize (MemoryLens [] cur post) = max cur (maximum post)
largestBlockSize (MemoryLens pre cur post) = max (maximum pre) $ max cur (maximum post)

redistributeCurrent :: MemoryLens -> MemoryLens
redistributeCurrent (MemoryLens pre n post) = redistribute n (MemoryLens pre 0 post)

redistribute :: Int -> MemoryLens -> MemoryLens
redistribute n = if n == 0 then id else redistribute (n-1) . incrementCurrentBlock . moveRight

moveRight (MemoryLens pre cur []) = MemoryLens [] b bs where
  (b:bs) = reverse (cur:pre)
moveRight (MemoryLens pre cur (p:post)) = MemoryLens (cur:pre) p post

moveLeft (MemoryLens [] cur post) = MemoryLens bs b [] where
  (b:bs) = reverse (cur:post)
moveLeft (MemoryLens (p:pre) cur post) = MemoryLens (pre) p (cur:post)

currentBlockSize (MemoryLens _ c _) = c
incrementCurrentBlock (MemoryLens pre c post) = MemoryLens pre (c+1) post

moveLeftmost m@(MemoryLens [] cur post) = m
moveLeftmost m = moveLeftmost . moveLeft $ m