module AoC.Generator.Core (
  Generator, makeSimpleGenerator, makeFilterGenerator,
  generateNext, generateAll, generateAllValues, value) where

data Generator = Generator { genState :: Int, generator :: Int -> Int }

makeSimpleGenerator s m = generateNext $ Generator { genState = s, generator = simpleGenerator m }
makeFilterGenerator s d m = generateNext $ Generator { genState = s, generator = filterGenerator m d }

generateNext :: Generator -> Generator
generateNext g = Generator { genState = generator g (genState g), generator = generator g }

simpleGenerator :: Int -> Int -> Int
simpleGenerator m state = (state*m) `mod` 2147483647

filterGenerator :: Int -> Int -> Int -> Int
filterGenerator m d state = let
  next = simpleGenerator m state in
    if next `mod` d == 0
    then next
    else filterGenerator m d next

value (Generator state _) = state

generateAll :: Generator -> [Generator]
generateAll = iterate generateNext
generateAllValues = map value . generateAll
