module AoC.Sporifica.Parse (parseGrid) where

import AoC.Sporifica.Core
import AoC.Misc (mapLeft)

import Text.Parsec
import Text.Parsec.String

parseGrid :: String -> Either String Nodes
parseGrid = mapLeft show . parse nodes ""

nodes :: Parser Nodes
nodes = do
  rows <- row `sepEndBy` char '\n'
  return $ makeGrid rows

row :: Parser [Node]
row = many1 node

node :: Parser Node
node = Clean <$ char '.' <|>
       Infected <$ char '#'

