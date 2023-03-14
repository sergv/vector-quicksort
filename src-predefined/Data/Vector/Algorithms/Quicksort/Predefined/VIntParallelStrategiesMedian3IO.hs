----------------------------------------------------------------------------
-- |
-- Module      :  Data.Vector.Algorithms.Quicksort.Predefined.VIntParallelStrategiesMedian3IO
-- Copyright   :  (c) Sergey Vinokurov 2023
-- License     :  Apache-2.0 (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

module Data.Vector.Algorithms.Quicksort.Predefined.VIntParallelStrategiesMedian3IO
  ( sortVIntParallelStrategiesMedian3IO
  ) where

import Control.Monad.ST
import Data.Int
import Data.Vector qualified as V

import Data.Vector.Algorithms.Quicksort.Parameterised

import Data.Vector.Algorithms.Quicksort.Predefined.VIntSequentialMedian3IO ()

{-# NOINLINE sortVIntParallelStrategiesMedian3IO #-}
sortVIntParallelStrategiesMedian3IO :: V.MVector RealWorld Int64 -> IO ()
sortVIntParallelStrategiesMedian3IO = sortFM ParStrategies (Median3 @Int64)

