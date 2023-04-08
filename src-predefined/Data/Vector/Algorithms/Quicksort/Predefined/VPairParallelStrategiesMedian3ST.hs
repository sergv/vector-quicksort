-- |
-- Module:     Data.Vector.Algorithms.Quicksort.Predefined.VPairParallelStrategiesMedian3ST
-- Copyright:  (c) Sergey Vinokurov 2023
-- License:    Apache-2.0 (see LICENSE)
-- Maintainer: serg.foo@gmail.com

module Data.Vector.Algorithms.Quicksort.Predefined.VPairParallelStrategiesMedian3ST
  ( sortVPairParallelStrategiesMedian3ST
  ) where

import Control.Monad.ST
import Data.Int
import Data.Vector qualified as V

import Data.Vector.Algorithms.Quicksort.Parameterised

import Data.Vector.Algorithms.Quicksort.Predefined.Pair
import Data.Vector.Algorithms.Quicksort.Predefined.VPairSequentialMedian3ST ()

{-# NOINLINE sortVPairParallelStrategiesMedian3ST #-}
sortVPairParallelStrategiesMedian3ST :: V.MVector s (TestPair Int32 Int32) -> ST s ()
sortVPairParallelStrategiesMedian3ST = sortFM ParStrategies (Median3 @(TestPair Int32 Int32))
