module Tests.AoC.ParticleSwarm (describeParticleSwarm) where

import AoC.ParticleSwarm.Core
import AoC.ParticleSwarm.Parse

import Test.Hspec

testParticle x x' x'' = fmap (\n -> (n,0,0)) $ Particle x x' x''

findCollisionByBrute :: Particle3d -> Particle3d -> Maybe Int
findCollisionByBrute a b = findCollision a b 0 where
  findCollision a b t = if (pos a) == (pos b) then Just t else findCollision (step a) (step b) (t+1)
  step p = p { pos = (px+vx+ax, py+vy+ay, pz+vz+az), vel = (vx+ax, vy+ay, vz+az) } where
    (px, py, pz) = pos p
    (vx, vy, vz) = vel p
    (ax, ay, az) = acc p

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

    it "should find 2 remaining particle in modified sample particles" $ do
      let sampleParticles = [testParticle (-6) 3 0,
                            testParticle (-3) 2 0,
                            testParticle (-2) 1 0,
                            testParticle 3 (-1) 0]
      findRemainingAfterCollisions sampleParticles `shouldBe` 2
  
  
  describe "computeCollisions" $ do
    it "should find t=0 for two stationary particles at the same position" $ do
      let sampleParticles = [testParticle 2 0 0, testParticle 2 0 0]
      computeCollisions sampleParticles `shouldBe` [(0, (1,2))]
    
    it "should find t=1 for two non-accelerating particles" $ do
      let sampleParticles = [testParticle 2 1 0, testParticle 1 2 0]
      computeCollisions sampleParticles `shouldBe` [(1,(1,2))]
    
  describe "computeCollisionTime" $ do
    it "should find t=1 for two non-accelerating particles" $ do
      let p0 = testParticle 2 1 0
      let p1 = testParticle 1 2 0
      computeCollisionTime p0 p1 `shouldBe` Just 1
    
    it "should find t = 2 for accelerating particles" $ do
      let p0 = testParticle (-2) 1 0 -- (-2,1) (-1,1) (0,1)   (1,1)  (2,1) (3,1) (4,1) (5,1)
      let p1 = testParticle 5 (-4) 1 -- (5,-4) (2,-3) (0,-1) (0,0) (1,1) (2,1) (4,2) (7,1)
      computeCollisionTime p0 p1 `shouldBe` Just 2
    
    it "should find correct times some selected " $ do
      let p0 = testParticle 6 (-4) 2  -- (6,-4) (4,-2) (4,0) (6,2) (10,4) (16,6)
      let p1 = testParticle 0 8 (-3)  -- (0,8) (5,5)  (7,2) (6,-1) (2,-4) (-5,-7)
      computeCollisionTime p0 p1 `shouldBe` findCollisionByBrute p0 p1
