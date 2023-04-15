-- |
-- Module:     Data.Vector.Algorithms.Quicksort.Predefined.VIntParallelStrategiesMedian3or5IO
-- Copyright:  (c) Sergey Vinokurov 2023
-- License:    Apache-2.0 (see LICENSE)
-- Maintainer: serg.foo@gmail.com

module Data.Vector.Algorithms.Quicksort.Predefined.VIntParallelStrategiesMedian3or5IO
  ( sortVIntParallelStrategiesMedian3or5IO
  ) where

import Control.Monad.ST
import Data.Int
import Data.Vector qualified as V

import Data.Vector.Algorithms.Quicksort.Parameterised

import Data.Vector.Algorithms.Quicksort.Predefined.VIntSequentialMedian3IO ()

{-# NOINLINE sortVIntParallelStrategiesMedian3or5IO #-}
sortVIntParallelStrategiesMedian3or5IO :: V.MVector RealWorld Int64 -> IO ()
sortVIntParallelStrategiesMedian3or5IO = sortInplaceFM defaultParStrategies (Median3or5 @Int64)


