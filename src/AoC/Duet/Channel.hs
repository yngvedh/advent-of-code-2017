module AoC.Duet.Channel (
  Channel (..), IoReg (..), IoBuf (..)) where

class Channel a where
  channelRead :: a -> (Int , a)
  channelWrite :: Int -> a -> a
  makeChannel :: a
  hasInput :: a -> Bool


data IoReg = IoReg Int
  deriving (Show, Eq)

channelRegRead :: IoReg -> (Int, IoReg)
channelRegRead r@(IoReg n) = (n, r)

channelRegWrite :: Int -> IoReg -> IoReg
channelRegWrite n _ = IoReg n

data IoBuf = IoBuf [Int]
  deriving (Show, Eq)

channelBufRead :: IoBuf -> (Int, IoBuf)
channelBufRead (IoBuf ns) = (last ns, IoBuf $ init ns)

channelBufWrite :: Int -> IoBuf -> IoBuf
channelBufWrite n (IoBuf ns) = IoBuf (n:ns)

instance Channel IoReg where
  channelRead = channelRegRead
  channelWrite = channelRegWrite
  makeChannel = IoReg 0
  hasInput _ = True

instance Channel IoBuf where
  channelRead = channelBufRead
  channelWrite = channelBufWrite
  makeChannel = IoBuf []
  hasInput (IoBuf []) = False
  hasInput _ = True
