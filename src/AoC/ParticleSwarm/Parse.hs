module AoC.ParticleSwarm.Parse (parseParticles) where

import AoC.ParticleSwarm.Core

import Text.Parsec
import Text.Parsec.String

import AoC.Misc (mapLeft)
import AoC.ParsePrimitives

parseParticles :: String -> Either String [Particle3d]
parseParticles = mapLeft show . parse particles ""

particles :: Parser [Particle3d]
particles = particle `sepEndBy` char '\n'

-- p=<4459,-902,279>, v=<-48,66,-14>, a=<-15,-2,0>
particle :: Parser Particle3d
particle = do
  string "p="
  p <- xyz
  string ", v="
  v <- xyz
  string ", a="
  a <- xyz
  return $ Particle p v a

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
