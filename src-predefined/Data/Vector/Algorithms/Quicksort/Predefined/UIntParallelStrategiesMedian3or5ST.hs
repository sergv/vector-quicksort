-- |
-- Module:     Data.Vector.Algorithms.Quicksort.Predefined.UIntParallelStrategiesMedian3or5ST
-- Copyright:  (c) Sergey Vinokurov 2023
-- License:    Apache-2.0 (see LICENSE)
-- Maintainer: serg.foo@gmail.com

module Data.Vector.Algorithms.Quicksort.Predefined.UIntParallelStrategiesMedian3or5ST
  ( sortUIntParallelStrategiesMedian3or5ST
  ) where

import Control.Monad.ST
import Data.Int
import Data.Vector.Unboxed qualified as U

import Data.Vector.Algorithms.Quicksort.Parameterised

import Data.Vector.Algorithms.Quicksort.Predefined.UIntSequentialMedian3ST ()

{-# NOINLINE sortUIntParallelStrategiesMedian3or5ST #-}
sortUIntParallelStrategiesMedian3or5ST :: U.MVector s Int64 -> ST s ()
sortUIntParallelStrategiesMedian3or5ST = sortInplaceFM defaultParStrategies (Median3or5 @Int64)

