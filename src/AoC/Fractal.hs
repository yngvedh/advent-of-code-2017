module AoC.Fractal (parseRules, iterateRuleSet, countLitPixels) where

import qualified AoC.Fractal.Core as C
import qualified AoC.Fractal.Parse as P

parseRules :: String -> Either String C.RuleSet
parseRules = P.parseRules

iterateRuleSet :: C.RuleSet -> Int -> C.Image
iterateRuleSet = C.iterateRuleSet

countLitPixels :: C.Image -> Int
countLitPixels = length . filter ((==) C.On) . concat . C.imagePixels