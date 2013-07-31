module Main (main) where

import Test.Framework (defaultMain)

import qualified Subspace.DHT.Node.Tests

main = defaultMain
    [ Subspace.DHT.Node.Tests.tests
    ]