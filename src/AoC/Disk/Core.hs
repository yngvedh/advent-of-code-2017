module AoC.Disk.Core (Disk, diskFromKey, showDisk, countRegions, colorRegions, fillRegion) where

import AoC.KnotHash.Parse
import AoC.KnotHash.Core
import AoC.Focus.ListMatrix

import Data.Word
import Data.Maybe (catMaybes)

data Disk = Disk [[Word8]]

diskFromKey :: String -> Disk
diskFromKey key = Disk $ map fullHash keys where
  keys = map (codes . makeKey) [0..127]
  makeKey n = key ++ "-" ++ (show n)

showDisk :: Disk -> String
showDisk (Disk hashes) = unlines . map (showCustomBinaryHash bitChar) $ hashes
bitMatrix = map (map ((==) '#')) . lines . showDisk

bitChar :: Bool -> Char
bitChar True = '#'
bitChar False = '.'

countRegions :: Disk -> Int
countRegions = succ . maximum . catMaybes . concat . colorRegions

colorRegions :: Disk -> [[Maybe Int]]
colorRegions d = map (map justFromRight) . unfocus . colorRegions' 0 $ makeFocus m where
  m = map (map (Left :: Bool -> Either Bool Int)) . bitMatrix $ d
  colorRegions' :: Int -> ListMatrixFocus (Either Bool Int) -> ListMatrixFocus (Either Bool Int)
  colorRegions' nextId f = case focusNewRegion f of
    Just f' -> colorRegions' (nextId+1) . fillRegion nextId $ f'
    Nothing -> f
  focusNewRegion = focusElement ((==) $ Left True)
  
fillRegion id f = case get f of
  Right id -> f
  Left False -> f
  Left True -> fill isLeftMost moveLeft moveRight .
               fill isBottomMost moveDown moveUp .
               fill isRightMost moveRight moveLeft .
               fill isTopMost moveUp moveDown .
               set (Right id) $ f
               where
                 fill atEnd move unmove f = if atEnd f
                   then f
                   else unmove . fillRegion id . move $ f

justFromRight (Right n) = Just n
justFromRight (Left _) = Nothing
