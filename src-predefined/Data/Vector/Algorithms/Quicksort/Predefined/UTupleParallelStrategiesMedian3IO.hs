-- |
-- Module:     Data.Vector.Algorithms.Quicksort.Predefined.UTupleParallelStrategiesMedian3IO
-- Copyright:  (c) Sergey Vinokurov 2023
-- License:    Apache-2.0 (see LICENSE)
-- Maintainer: serg.foo@gmail.com

module Data.Vector.Algorithms.Quicksort.Predefined.UTupleParallelStrategiesMedian3IO
  ( sortUTupleParallelStrategiesMedian3IO
  ) where

import Control.Monad.ST
import Data.Int
import Data.Vector.Unboxed qualified as U

import Data.Vector.Algorithms.Quicksort.Parameterised

import Data.Vector.Algorithms.Quicksort.Predefined.UTupleSequentialMedian3IO ()

{-# NOINLINE sortUTupleParallelStrategiesMedian3IO #-}
sortUTupleParallelStrategiesMedian3IO :: U.MVector RealWorld (Int32, Int32) -> IO ()
sortUTupleParallelStrategiesMedian3IO = sortInplaceFM defaultParStrategies (Median3 @(Int32, Int32))

