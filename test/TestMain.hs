-- |
-- Module:     TestMain
-- Copyright:  (c) Sergey Vinokurov 2023
-- License:    Apache-2.0 (see LICENSE)
-- Maintainer: serg.foo@gmail.com

module TestMain (main) where

import Test.Tasty

import Data.Vector.Algorithms.Quicksort.Tests qualified

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
  [ Data.Vector.Algorithms.Quicksort.Tests.tests
  ]

