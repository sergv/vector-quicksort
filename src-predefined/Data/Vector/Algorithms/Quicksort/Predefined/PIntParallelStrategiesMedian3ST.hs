-- |
-- Module:     Data.Vector.Algorithms.Quicksort.Predefined.VIntParallelStrategiesMedian3ST
-- Copyright:  (c) Sergey Vinokurov 2023
-- License:    Apache-2.0 (see LICENSE)
-- Maintainer: serg.foo@gmail.com

module Data.Vector.Algorithms.Quicksort.Predefined.PIntParallelStrategiesMedian3ST
  ( sortPIntParallelStrategiesMedian3ST
  ) where

import Control.Monad.ST
import Data.Int
import Data.Vector.Primitive qualified as P

import Data.Vector.Algorithms.Quicksort.Parameterised

import Data.Vector.Algorithms.Quicksort.Predefined.PIntSequentialMedian3ST ()

{-# NOINLINE sortPIntParallelStrategiesMedian3ST #-}
sortPIntParallelStrategiesMedian3ST :: P.MVector s Int64 -> ST s ()
sortPIntParallelStrategiesMedian3ST = sortInplaceFM defaultParStrategies (Median3 @Int64)
