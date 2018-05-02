module AoC.Disk.Parse (parseKey) where

parseKey :: String -> Either String String
parseKey = Right . filter ((/=) '\n')