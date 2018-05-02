module AoC.KnotHash.Core (hash, fullHash, showFullHash, showBinaryHash, showCustomBinaryHash, hashBits) where

import Data.Char (ord, chr)
import Data.Word
import Data.Bits (xor, testBit)

hash :: Int -> [Int] -> Int
hash knotSize lengths = hashFromKnot $ applyTwists [0..(knotSize-1)] lengths 0 0

hashFromKnot (k1:k2:_) = k1*k2

applyTwists :: [Int] -> [Int] -> Int -> Int -> [Int]
applyTwists knot [] _ _ = knot
applyTwists knot (l:ls) skip pos = applyTwists knot' ls skip' pos' where
  knot' = applyTwist knot pos l
  pos' = (pos+l+skip) `mod` (length knot)
  skip' = skip+1

applyTwist knot pos len = knot' where
  normalizedKnot = rotate (-pos) knot
  (part1, part2) = splitAt len normalizedKnot
  normalizedKnot' = (reverse part1) ++ part2
  knot' = rotate pos normalizedKnot'

rotate :: Int -> [a] -> [a]
rotate offset = rot' (-offset) where
  rot' offset' list =
    if offset' < 0 then
      rot' (offset' + (length list)) list
    else
      post ++ pre where
        (pre, post) = splitAt offset' list

fullHash :: [Int] -> [Word8]
fullHash lengths = fullHashFromKnot $ applyTwists [0..255] lengths' 0 0 where
  lengths' = concat . replicate 64 $ lengths ++ [17, 31, 73, 47, 23]

fullHashFromKnot :: [Int] -> [Word8]
fullHashFromKnot = fullHashFromKnot' . map toEnum where
  fullHashFromKnot' [] = []
  fullHashFromKnot' knot = (foldl xor 0 $ take 16 knot):(fullHashFromKnot' $ drop 16 knot)

showFullHash :: [Word8] -> String
showFullHash = concat . map showHexByte

showHexByte :: Word8 -> String
showHexByte b = [hexChr b2, hexChr b1] where
  b1 = b' `mod` 16
  b2 = b' `div` 16
  b' = fromEnum b
  hexChr n = chr n' where
    n' = if n < 10 then n + (ord '0') else n - 10 + (ord 'a')

hashBits :: [Word8] -> [Bool]
hashBits = concat . map byteBits

byteBits :: Word8 -> [Bool]
byteBits b = map (testBit b) . reverse $ [0..7]

showBinaryHash :: [Word8] -> String
showBinaryHash = showCustomBinaryHash numericBitChar

showCustomBinaryHash :: (Bool -> Char) -> [Word8] -> String
showCustomBinaryHash bitChar = map bitChar . hashBits

showBinaryByte :: (Bool -> Char) -> Word8 -> String
showBinaryByte bitChar = map bitChar . byteBits

numericBitChar :: Bool -> Char
numericBitChar True = '1'
numericBitChar False = '0'
