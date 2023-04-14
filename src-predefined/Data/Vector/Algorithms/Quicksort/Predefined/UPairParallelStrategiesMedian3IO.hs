-- |
-- Module:     Data.Vector.Algorithms.Quicksort.Predefined.UPairParallelStrategiesMedian3IO
-- Copyright:  (c) Sergey Vinokurov 2023
-- License:    Apache-2.0 (see LICENSE)
-- Maintainer: serg.foo@gmail.com

module Data.Vector.Algorithms.Quicksort.Predefined.UPairParallelStrategiesMedian3IO
  ( sortUPairParallelStrategiesMedian3IO
  ) where

import Control.Monad.ST
import Data.Int
import Data.Vector.Unboxed qualified as U

import Data.Vector.Algorithms.Quicksort.Parameterised

import Data.Vector.Algorithms.Quicksort.Predefined.Pair
import Data.Vector.Algorithms.Quicksort.Predefined.UPairSequentialMedian3IO ()

{-# NOINLINE sortUPairParallelStrategiesMedian3IO #-}
sortUPairParallelStrategiesMedian3IO :: U.MVector RealWorld (TestPair Int32 Int32) -> IO ()
sortUPairParallelStrategiesMedian3IO = sortInplaceFM ParStrategies (Median3 @(TestPair Int32 Int32))

