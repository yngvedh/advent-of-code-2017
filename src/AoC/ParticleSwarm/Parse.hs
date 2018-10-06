module AoC.ParticleSwarm.Parse (parseParticles) where

import AoC.ParticleSwarm.Core

import Text.Parsec
import Text.Parsec.String

import AoC.Misc (mapLeft)
import AoC.ParsePrimitives

parseParticles :: String -> Either String [Particle]
parseParticles = mapLeft show . parse particles ""

particles :: Parser [Particle]
particles = particle `sepEndBy` char '\n'

-- p=<4459,-902,279>, v=<-48,66,-14>, a=<-15,-2,0>
particle :: Parser Particle
particle = do
  string "p="
  (px,py,pz) <- xyz
  string ", v="
  (vx,vy,vz) <- xyz
  string ", a="
  (ax,ay,az) <- xyz
  return $ Particle (Pos px py pz) (Vel vx vy vz) (Acc ax ay az)

xyz :: Parser (Int,Int,Int)
xyz = do
  char '<'
  x <- integer
  char ','
  y <- integer
  char ','
  z <- integer
  char '>'
  return (x,y,z)
