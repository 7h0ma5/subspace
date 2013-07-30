module Subspace.DHT.Crypto where

import System.Random
import Codec.Crypto.RSA
import Subspace.DHT.Node (NodeId)

data KeyPair = KeyPair (PublicKey, PrivateKey)

generateNodeId :: NodeId
generateNodeId = undefined

generateIdentity :: Int
generateIdentity = do
  rand <- getStdGen
  let (pubKey, privKey, g) = generateKeyPair rand 1024
  (pubKey, privKey)