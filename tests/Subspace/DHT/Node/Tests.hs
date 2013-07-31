module Subspace.DHT.Node.Tests (tests) where

import Test.Framework
import Test.QuickCheck
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Subspace.DHT.Node
import Data.Binary
import Data.LargeWord
import Network.Socket

tests :: Test
tests = testGroup "Subspace.DHT.Node.Tests"
    [ testProperty "binary preservation" binaryPreservation ]

binaryPreservation :: Word64 -> Word16 -> Word32 -> Bool
binaryPreservation nodeId port addr = newNode == origNode
   where newNode = decode bin :: Node
         bin = encode origNode
         origNode = Node i (SockAddrInet (PortNum port) addr)
         i = LargeKey nodeId (LargeKey nodeId (LargeKey nodeId nodeId))