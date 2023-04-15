-- |
-- Module:     Data.Vector.Algorithms.Quicksort.Predefined.VPairParallelStrategiesMedian3IO
-- Copyright:  (c) Sergey Vinokurov 2023
-- License:    Apache-2.0 (see LICENSE)
-- Maintainer: serg.foo@gmail.com

module Data.Vector.Algorithms.Quicksort.Predefined.VPairParallelStrategiesMedian3IO
  ( sortVPairParallelStrategiesMedian3IO
  ) where

import Control.Monad.ST
import Data.Int
import Data.Vector qualified as V

import Data.Vector.Algorithms.Quicksort.Parameterised

import Data.Vector.Algorithms.Quicksort.Predefined.Pair
import Data.Vector.Algorithms.Quicksort.Predefined.VPairSequentialMedian3IO ()

{-# NOINLINE sortVPairParallelStrategiesMedian3IO #-}
sortVPairParallelStrategiesMedian3IO :: V.MVector RealWorld (TestPair Int32 Int32) -> IO ()
sortVPairParallelStrategiesMedian3IO = sortInplaceFM defaultParStrategies (Median3 @(TestPair Int32 Int32))
