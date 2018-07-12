module AoC.BlockMemory (Memory, makeMemory, balanceStep, balance) where

import Data.List (unfoldr, elemIndex)

data Memory = Memory [Int]
  deriving (Eq, Show)

data MemoryLens = MemoryLens [Int] Int [Int]

makeMemory :: [Int] -> Memory
makeMemory = Memory

asLens :: Memory -> MemoryLens
asLens (Memory (b:bs)) = MemoryLens [] b bs where
asLens (Memory []) = error "Attempt to create lens from empty list"

asMemory :: MemoryLens -> Memory
asMemory (MemoryLens front cur back) = Memory $ (reverse front) ++ (cur:back)

balance :: Memory -> [Memory]
balance  = map asMemory . balanceLens . asLens

balanceStep :: Memory -> Memory
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
    (Just i) = elemIndex (maximum bs) bs
    pre = reverse $ take i bs
    cur = head $ drop i bs
    post = tail $ drop i bs

redistributeCurrent :: MemoryLens -> MemoryLens
redistributeCurrent (MemoryLens pre n post) = redistribute n (MemoryLens pre 0 post)

redistribute :: Int -> MemoryLens -> MemoryLens
redistribute n = if n == 0 then id else redistribute (n-1) . incrementCurrentBlock . moveRight

moveRight :: MemoryLens -> MemoryLens
moveRight (MemoryLens pre cur []) = MemoryLens [] b bs where
  (b:bs) = reverse (cur:pre)
moveRight (MemoryLens pre cur (p:post)) = MemoryLens (cur:pre) p post

incrementCurrentBlock :: MemoryLens -> MemoryLens
incrementCurrentBlock (MemoryLens pre c post) = MemoryLens pre (c+1) post
