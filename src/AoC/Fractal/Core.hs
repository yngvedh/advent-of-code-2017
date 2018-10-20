module AoC.Fractal.Core (
  Rule (..), RuleSet (..), Image (..), Pixel (..),
  makeRuleSet,
  iterateRuleSet,
  applyRuleSet,
  toPixel,
  divideImage, joinImage,
  rotateImage, mirrorImage,
  imagePixels
) where

import Data.Maybe (fromJust)
import Data.List (find, transpose)

import AoC.Misc (mapFst, mapSnd)

import Debug.Trace(trace, traceShow)

data Pixel = On | Off
  deriving (Show, Eq)
data Rule = Rule Image Image
  deriving (Show, Eq)
data RuleSet = RuleSet [Rule]
  deriving (Show, Eq)
data Image = Image [[Pixel]]
  deriving (Show, Eq)

initialImage = Image 
  [[Off,On,Off]
  ,[Off,Off,On]
  ,[On,On,On]]

makeRuleSet :: [([String],[String])] -> RuleSet
makeRuleSet = RuleSet . map (toRule . mapFst toImage . mapSnd toImage) where
  toRule (from,to) = Rule from to
  toImage = Image . map (map toPixel)

toPixel :: Char -> Pixel
toPixel '.' = Off
toPixel '#' = On

iterateRuleSet :: RuleSet -> Int -> Image
iterateRuleSet rs n = flip (!!) n . iterate (evolve rs) $ initialImage

evolve :: RuleSet -> Image -> Image
evolve rules = joinImage . map (applyRuleSet rules) . divideImage

divides :: Int -> Int -> Bool
divides d n = n `mod` d == 0

imagePixels :: Image -> [[Pixel]]
imagePixels (Image pxs) = pxs

divideImage :: Image -> [Image]
divideImage (Image pxs) = pieces where
  pieceSize = if divides 2 $ length pxs then 2 else 3
  pieces = map Image . concat . map joinColumns $ splitColsAndRows
  splitColsAndRows :: [[[[Pixel]]]]
  splitColsAndRows = divideList pieceSize splitCols
  splitCols :: [[[Pixel]]]
  splitCols = map (divideList pieceSize) pxs
  joinColumns :: [[[Pixel]]] -> [[[Pixel]]]
  joinColumns cs | null . head $ cs = []
                 | otherwise        = (map head cs):(joinColumns . map tail $ cs)

divideList :: Int -> [a] -> [[a]]
divideList _ [] = []
divideList n list = (take n list) : (divideList n . drop n $ list)

joinImage :: [Image] -> Image
joinImage pieces = Image imgRows' where
  imgRows' = concat . map imagePixels $ joinedPieceRows
  joinedPieceRows = map joinRowOfPieces pieceRows
  pieceRows = divideList piecesInRow pieces
  piecesInRow = case find (\x -> x*x == numPieces) $ [1..numPieces] of
                  Just n  -> n
                  Nothing -> error $ "Could not find sqrt of #pieces:" ++ show numPieces
  numPieces = length pieces

joinRowOfPieces :: [Image] -> Image
joinRowOfPieces is = Image . foldl1 (zipWith (++)) . map imagePixels $ is where

applyRuleSet :: RuleSet -> Image -> Image
applyRuleSet rs img = enhancement where
  (Rule _ enhancement) = findMatchingRule rs img

findMatchingRule :: RuleSet -> Image -> Rule
findMatchingRule (RuleSet rs) img =
  case find (flip matchesRule img) $ rs of
    Just rule -> rule
    Nothing   -> error $ "Could not find matching rule for image: " ++ show img

matchesRule :: Rule -> Image -> Bool
matchesRule rule@(Rule pattern _) img0 = pattern `elem` is where
  img1 = rotateImage img0
  img2 = rotateImage img1
  img3 = rotateImage img2
  imgs = [img0, img1, img2, img3]
  imgs' = map mirrorImage imgs
  is = imgs ++ imgs'

rotateImage = Image . reverse . transpose . imagePixels
mirrorImage = Image . reverse . imagePixels

-- rotateImage (Image [[a,b], [c,d]]) = Image [[c, a], [d, b]]
-- rotateImage (Image [[a,b,c],[e,f,g],[h,i,j]]) =  Image [[h,e,a],[i,f,b],[j,g,c]]
-- rotateImage img = error $ "rotateImage: Unsupported size of image " ++ show img

