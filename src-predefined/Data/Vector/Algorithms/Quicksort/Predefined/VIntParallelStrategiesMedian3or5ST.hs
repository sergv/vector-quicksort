-- |
-- Module:     Data.Vector.Algorithms.Quicksort.Predefined.VIntParallelStrategiesMedian3or5ST
-- Copyright:  (c) Sergey Vinokurov 2023
-- License:    Apache-2.0 (see LICENSE)
-- Maintainer: serg.foo@gmail.com

module Data.Vector.Algorithms.Quicksort.Predefined.VIntParallelStrategiesMedian3or5ST
  ( sortVIntParallelStrategiesMedian3or5ST
  ) where

import Control.Monad.ST
import Data.Int
import Data.Vector qualified as V

import Data.Vector.Algorithms.Quicksort.Parameterised

import Data.Vector.Algorithms.Quicksort.Predefined.VIntSequentialMedian3ST ()

{-# NOINLINE sortVIntParallelStrategiesMedian3or5ST #-}
sortVIntParallelStrategiesMedian3or5ST :: V.MVector s Int64 -> ST s ()
sortVIntParallelStrategiesMedian3or5ST = sortFM ParStrategies (Median3or5 @Int64)

