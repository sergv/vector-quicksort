----------------------------------------------------------------------------
-- |
-- Module      :  Data.Vector.Algorithms.Quicksort.Predefined.VTupleParallelStrategiesMedian3IO
-- Copyright   :  (c) Sergey Vinokurov 2023
-- License     :  Apache-2.0 (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

module Data.Vector.Algorithms.Quicksort.Predefined.VTupleParallelStrategiesMedian3IO
  ( sortVTupleParallelStrategiesMedian3IO
  ) where

import Control.Monad.ST
import Data.Int
import Data.Vector qualified as V

import Data.Vector.Algorithms.Quicksort.Parameterised

import Data.Vector.Algorithms.Quicksort.Predefined.VTupleSequentialMedian3IO ()

{-# NOINLINE sortVTupleParallelStrategiesMedian3IO #-}
sortVTupleParallelStrategiesMedian3IO :: V.MVector RealWorld (Int32, Int32) -> IO ()
sortVTupleParallelStrategiesMedian3IO = sortFM ParStrategies (Median3 @(Int32, Int32))
