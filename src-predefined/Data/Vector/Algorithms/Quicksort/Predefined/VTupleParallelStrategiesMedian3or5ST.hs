-- |
-- Module:     Data.Vector.Algorithms.Quicksort.Predefined.VTupleParallelStrategiesMedian3or5ST
-- Copyright:  (c) Sergey Vinokurov 2023
-- License:    Apache-2.0 (see LICENSE)
-- Maintainer: serg.foo@gmail.com

module Data.Vector.Algorithms.Quicksort.Predefined.VTupleParallelStrategiesMedian3or5ST
  ( sortVTupleParallelStrategiesMedian3or5ST
  ) where

import Control.Monad.ST
import Data.Int
import Data.Vector qualified as V

import Data.Vector.Algorithms.Quicksort.Parameterised

import Data.Vector.Algorithms.Quicksort.Predefined.VTupleSequentialMedian3ST ()

{-# NOINLINE sortVTupleParallelStrategiesMedian3or5ST #-}
sortVTupleParallelStrategiesMedian3or5ST :: V.MVector s (Int32, Int32) -> ST s ()
sortVTupleParallelStrategiesMedian3or5ST = sortInplaceFM defaultParStrategies (Median3or5 @(Int32, Int32))


