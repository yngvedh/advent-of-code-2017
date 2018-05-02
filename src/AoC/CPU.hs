module AoC.CPU (Jumps, makeJumps, makeJumpsOnly, makeInitialJumps, step, weirdStep, isCpuOutside) where

data Jumps = Jumps { precedingJumps :: [Int], jumpInstruction :: (Maybe Int), succeedingJumps :: [Int] }
  deriving (Eq, Show)

makeJumps :: [Int] -> Int -> [Int] -> Jumps
makeJumps ps j ss = Jumps ps (Just j) ss

makeInitialJumps :: [Int] -> Jumps
makeInitialJumps [] = makeJumpsOnly []
makeInitialJumps (x:xs) = makeJumps [] x xs

makeJumpsOnly :: [Int] -> Jumps
makeJumpsOnly js = Jumps js Nothing []

step :: Jumps -> Jumps
step = step' (+1)

weirdStep :: Jumps -> Jumps
weirdStep = step' newOffset where
  newOffset n = if n >= 3 then n-1 else n+1

step' :: (Int -> Int) -> Jumps -> Jumps
step' _ js@(Jumps _ Nothing _) = js
step' newOffset (Jumps ps (Just j) ss) = moveSp j $ makeJumps ps (newOffset j) ss

moveSp _ js@(Jumps _ Nothing _) = js
moveSp 0 js = js
moveSp n (Jumps ps (Just j) ss) =
  if n > 0 then
    if null ss then
      makeJumpsOnly (j:ps)
    else
      moveSp (n-1) $ makeJumps (j:ps) (head ss) (tail ss)
  else
    if null ps then
      makeJumpsOnly $ (j:(reverse ss))
    else
      moveSp (n+1) $ makeJumps (tail ps) (head ps) (j:ss)

isCpuOutside :: Jumps -> Bool
isCpuOutside (Jumps _ Nothing _) = True
isCpuOutside _ = False