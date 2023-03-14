----------------------------------------------------------------------------
-- |
-- Module      :  Data.Vector.Algorithms.Quicksort.Predefined.VIntParallelStrategiesMedian3or5ST
-- Copyright   :  (c) Sergey Vinokurov 2023
-- License     :  Apache-2.0 (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

module Data.Vector.Algorithms.Quicksort.Predefined.PIntParallelStrategiesMedian3or5IO
  ( sortPIntParallelStrategiesMedian3or5IO
  ) where

import Control.Monad.ST
import Data.Int
import Data.Vector.Primitive qualified as P

import Data.Vector.Algorithms.Quicksort.Parameterised

import Data.Vector.Algorithms.Quicksort.Predefined.PIntSequentialMedian3IO ()

{-# NOINLINE sortPIntParallelStrategiesMedian3or5IO #-}
sortPIntParallelStrategiesMedian3or5IO :: P.MVector RealWorld Int64 -> IO ()
sortPIntParallelStrategiesMedian3or5IO = sortFM ParStrategies (Median3or5 @Int64)

