-- |
-- Module:     Data.Vector.Algorithms.Quicksort.Predefined.VTupleParallelStrategiesMedian3ST
-- Copyright:  (c) Sergey Vinokurov 2023
-- License:    Apache-2.0 (see LICENSE)
-- Maintainer: serg.foo@gmail.com

module Data.Vector.Algorithms.Quicksort.Predefined.VTupleParallelStrategiesMedian3ST
  ( sortVTupleParallelStrategiesMedian3ST
  ) where

import Control.Monad.ST
import Data.Int
import Data.Vector qualified as V

import Data.Vector.Algorithms.Quicksort.Parameterised

import Data.Vector.Algorithms.Quicksort.Predefined.VTupleSequentialMedian3ST ()

{-# NOINLINE sortVTupleParallelStrategiesMedian3ST #-}
sortVTupleParallelStrategiesMedian3ST :: V.MVector s (Int32, Int32) -> ST s ()
sortVTupleParallelStrategiesMedian3ST = sortInplaceFM defaultParStrategies (Median3 @(Int32, Int32))
