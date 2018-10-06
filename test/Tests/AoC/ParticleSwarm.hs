module Tests.AoC.ParticleSwarm (describeParticleSwarm) where

import AoC.ParticleSwarm.Core
import AoC.ParticleSwarm.Parse

import Test.Hspec

testParticle x x' x'' = fmap (\n -> (n,0,0)) $ Particle x x' x'' where

describeParticleSwarm = describe "Tests.AoC.ParticleSwarm" $ do
  let sampleParticles = [testParticle 3 2 (-1),
                         testParticle 4 0 (-2)]

  describe "parse" $ do
    let sampleInput = unlines ["p=<3,0,0>, v=<2,0,0>, a=<-1,0,0>",
                               "p=<4,0,0>, v=<0,0,0>, a=<-2,0,0>",
                               ""]
    
    it "should parse sample correctly" $ do
      parseParticles sampleInput `shouldBe` Right sampleParticles
  
  describe "findClosestLongTerm" $ do
    it "should figure out that '0' is closest long term in sample particles" $ do
      findClosestLongTerm sampleParticles `shouldBe` 0
  
  describe "findRemainingAfterCollisions" $ do
    it "should find 1 remaining particle in sample particles" $ do
      let sampleParticles = [testParticle (-6) 3 0,
                            testParticle (-4) 2 0,
                            testParticle (-2) 1 0,
                            testParticle 3 (-1) 0]
      findRemainingAfterCollisions sampleParticles `shouldBe` 1
