module Subspace.DHT.Protocol where

import Subspace.DHT.Node
import Network.Socket
import Data.Word
import Data.LargeWord
import Data.Binary
import Data.Binary.Put
import Data.Binary.Get

type PingId = Word64

data Query = Ping Node PingId
           | Pong Node PingId
           | GetNodes Node
           | SendNodes Node

instance Show Query where
  show (Ping node id) = "(Ping " ++ show id ++ " from " ++ show node ++ ")"
  show (Pong node id) = "(Ping " ++ show id ++ " from " ++ show node ++ ")"
  show (GetNodes node) = "(GetNodes from " ++ show node ++ ")"
  show (SendNodes node) = "(SendNodes from " ++ show node ++ ")"

instance Binary Query where
  put (Ping node pingId) = do
    putWord8 0
    put node
    putWord64be pingId

  put (Pong node pingId) = do
    putWord8 1
    put node
    putWord64be pingId

  put (GetNodes node) = do
    putWord8 2

  put (SendNodes node) = do
    putWord8 3

  get = do
    qType <- getWord8
    case qType of
      0 -> do
        node <- get :: Get Node
        pingId <- get :: Get PingId
        return (Ping node pingId)
      1 -> do
        node <- get :: Get Node
        pingId <- get :: Get PingId
        return (Pong node pingId)
      2 -> do
        node <- get :: Get Node
        return (GetNodes node)
      3 -> do
        node <- get :: Get Node
        return (SendNodes node)
        
