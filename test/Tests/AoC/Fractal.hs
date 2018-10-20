module Tests.AoC.Fractal (describeFractal) where

import AoC.Fractal.Core
import AoC.Fractal.Parse

import Test.Hspec

describeFractal = describe "AoC.Fractal" $ do
  let sampleRules = makeRuleSet [(["..",".#"], ["##.","#..","..."])
                                ,([".#.","..#","###"], ["#..#","....","....","#..#"])]
  
  describe "parseRules" $ do
    it "parses sample input correctly" $ do
      let sampleInput = unlines ["../.# => ##./#../..."
                                ,".#./..#/### => #..#/..../..../#..#"]
      parseRules sampleInput `shouldBe` Right sampleRules
    
  describe "iterateFractal" $ do
    it "produces the sample image after 2 iterations" $ do
      let image' = ["##.##."
                   ,"#..#.."
                   ,"......"
                   ,"##.##."
                   ,"#..#.."
                   ,"......"]
      let image = Image . map (map toPixel) $ image'
      iterateRuleSet sampleRules 2 `shouldBe` image
  
  describe "divideImage, joinImage" $ do
    let image = Image
                 [[On,On,Off,On],
                  [On,On,On,Off],
                  [On,Off,Off,Off],
                  [Off,On,Off,Off]]
    let chunks = [Image [[On, On], [On, On]],
                  Image [[Off,On],[On,Off]],
                  Image [[On,Off],[Off,On]],
                  Image [[Off,Off],[Off,Off]]]
    describe "divideImage" $ do
      it "should divide 4x4 image into 4 pieces" $ do
        divideImage image `shouldBe` chunks
    
    describe "joinImage" $ do
      it "should join 4 2x2 pieces into 4x4 image" $ do
        joinImage chunks `shouldBe` image
  
  describe "rotateImage" $ do
    it "should rotate 2x2 correctly" $ do
      let image = Image [[On,Off],[Off,Off]]
      let images = take 5 . iterate rotateImage $ image
      let expected = [Image [[On,Off],[Off,Off]],
                      Image [[Off,Off],[On,Off]],
                      Image [[Off,Off],[Off,On]],
                      Image [[Off,On],[Off,Off]],
                      Image [[On,Off],[Off,Off]]]
      images `shouldBe` expected

    it "should rotate 3x3 correctly" $ do
      let image = Image [[On,Off,Off],[Off,Off,On],[Off,On,Off]]
      let images = take 5 . iterate rotateImage $ image
      let expected = [Image [[On,Off,Off],[Off,Off,On],[Off,On,Off]],
                      Image [[Off,On,Off],[Off,Off,On],[On,Off,Off]],
                      Image [[Off,On,Off],[On,Off,Off],[Off,Off,On]],
                      Image [[Off,Off,On],[On,Off,Off],[Off,On,Off]],
                      Image [[On,Off,Off],[Off,Off,On],[Off,On,Off]]]
      images `shouldBe` expected
        
  describe "applyRuleSet" $ do
    let expected3x3 = Image [[On,On,Off],[On,Off,Off],[Off,Off,Off]]
    let expected4x4 = Image [[On,Off,Off,On],[Off,Off,Off,Off],[Off,Off,Off,Off],[On,Off,Off,On]]

    it "should apply rules correctly to 2x2 image"  $ do
      let image = Image [[Off, Off], [Off, On]]
      applyRuleSet sampleRules image `shouldBe` expected3x3

    it "should apply rules correctly to rotated 2x2 image" $ do
      let image = Image [[Off, On], [Off, Off]]
      applyRuleSet sampleRules image `shouldBe` expected3x3
    
    it "should apply rules correctly to 3x3 image" $ do
      let image = Image [[Off,On,Off],[Off,Off,On],[On,On,On]]
      applyRuleSet sampleRules image `shouldBe` expected4x4

    it "should apply rules correctly to rotated 3x3 image" $ do
      let image = Image [[On,On,Off],[On,Off,On],[On,Off,Off]]
      applyRuleSet sampleRules image `shouldBe` expected4x4
    
