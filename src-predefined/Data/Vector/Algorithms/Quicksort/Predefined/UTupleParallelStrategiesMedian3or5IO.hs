-- |
-- Module:     Data.Vector.Algorithms.Quicksort.Predefined.UTupleParallelStrategiesMedian3or5IO
-- Copyright:  (c) Sergey Vinokurov 2023
-- License:    Apache-2.0 (see LICENSE)
-- Maintainer: serg.foo@gmail.com

module Data.Vector.Algorithms.Quicksort.Predefined.UTupleParallelStrategiesMedian3or5IO
  ( sortUTupleParallelStrategiesMedian3or5IO
  ) where

import Control.Monad.ST
import Data.Int
import Data.Vector.Unboxed qualified as U

import Data.Vector.Algorithms.Quicksort.Parameterised

import Data.Vector.Algorithms.Quicksort.Predefined.UTupleSequentialMedian3IO ()

{-# NOINLINE sortUTupleParallelStrategiesMedian3or5IO #-}
sortUTupleParallelStrategiesMedian3or5IO :: U.MVector RealWorld (Int32, Int32) -> IO ()
sortUTupleParallelStrategiesMedian3or5IO = sortInplaceFM defaultParStrategies (Median3or5 @(Int32, Int32))


