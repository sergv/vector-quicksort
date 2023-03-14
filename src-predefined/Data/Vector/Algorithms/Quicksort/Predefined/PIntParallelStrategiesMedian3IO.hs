----------------------------------------------------------------------------
-- |
-- Module      :  Data.Vector.Algorithms.Quicksort.Predefined.PIntParallelStrategiesMedian3IO
-- Copyright   :  (c) Sergey Vinokurov 2023
-- License     :  Apache-2.0 (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

module Data.Vector.Algorithms.Quicksort.Predefined.PIntParallelStrategiesMedian3IO
  ( sortPIntParallelStrategiesMedian3IO
  ) where

import Control.Monad.ST
import Data.Int
import Data.Vector.Primitive qualified as P

import Data.Vector.Algorithms.Quicksort.Parameterised

import Data.Vector.Algorithms.Quicksort.Predefined.PIntSequentialMedian3IO ()

{-# NOINLINE sortPIntParallelStrategiesMedian3IO #-}
sortPIntParallelStrategiesMedian3IO :: P.MVector RealWorld Int64 -> IO ()
sortPIntParallelStrategiesMedian3IO = sortFM ParStrategies (Median3 @Int64)

