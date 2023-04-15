-- |
-- Module:     Data.Vector.Algorithms.Quicksort.Predefined.UTupleParallelStrategiesMedian3ST
-- Copyright:  (c) Sergey Vinokurov 2023
-- License:    Apache-2.0 (see LICENSE)
-- Maintainer: serg.foo@gmail.com

module Data.Vector.Algorithms.Quicksort.Predefined.UTupleParallelStrategiesMedian3ST
  ( sortUTupleParallelStrategiesMedian3ST
  ) where

import Control.Monad.ST
import Data.Int
import Data.Vector.Unboxed qualified as U

import Data.Vector.Algorithms.Quicksort.Parameterised

import Data.Vector.Algorithms.Quicksort.Predefined.UTupleSequentialMedian3ST ()

{-# NOINLINE sortUTupleParallelStrategiesMedian3ST #-}
sortUTupleParallelStrategiesMedian3ST :: U.MVector s (Int32, Int32) -> ST s ()
sortUTupleParallelStrategiesMedian3ST = sortInplaceFM defaultParStrategies (Median3 @(Int32, Int32))
