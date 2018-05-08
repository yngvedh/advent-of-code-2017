module AoC.SpinLock.Parse (parseSkipDistance) where

parseSkipDistance :: String -> Either String Int
parseSkipDistance = Right . read