-- |
-- Module:     Data.Vector.Algorithms.Quicksort.Predefined.VIntParallelStrategiesMedian3ST
-- Copyright:  (c) Sergey Vinokurov 2023
-- License:    Apache-2.0 (see LICENSE)
-- Maintainer: serg.foo@gmail.com

module Data.Vector.Algorithms.Quicksort.Predefined.VIntParallelStrategiesMedian3ST
  ( sortVIntParallelStrategiesMedian3ST
  ) where

import Control.Monad.ST
import Data.Int
import Data.Vector qualified as V

import Data.Vector.Algorithms.Quicksort.Parameterised

import Data.Vector.Algorithms.Quicksort.Predefined.VIntSequentialMedian3ST ()

{-# NOINLINE sortVIntParallelStrategiesMedian3ST #-}
sortVIntParallelStrategiesMedian3ST :: V.MVector s Int64 -> ST s ()
sortVIntParallelStrategiesMedian3ST = sortInplaceFM defaultParStrategies (Median3 @Int64)


