-- |
-- Module:     Data.Vector.Algorithms.Quicksort.Predefined.UPairParallelStrategiesMedian3or5ST
-- Copyright:  (c) Sergey Vinokurov 2023
-- License:    Apache-2.0 (see LICENSE)
-- Maintainer: serg.foo@gmail.com

module Data.Vector.Algorithms.Quicksort.Predefined.UPairParallelStrategiesMedian3or5ST
  ( sortUPairParallelStrategiesMedian3or5ST
  ) where

import Control.Monad.ST
import Data.Int
import Data.Vector.Unboxed qualified as U

import Data.Vector.Algorithms.Quicksort.Parameterised

import Data.Vector.Algorithms.Quicksort.Predefined.Pair
import Data.Vector.Algorithms.Quicksort.Predefined.UPairSequentialMedian3ST ()

{-# NOINLINE sortUPairParallelStrategiesMedian3or5ST #-}
sortUPairParallelStrategiesMedian3or5ST :: U.MVector s (TestPair Int32 Int32) -> ST s ()
sortUPairParallelStrategiesMedian3or5ST = sortInplaceFM defaultParStrategies (Median3or5 @(TestPair Int32 Int32))


