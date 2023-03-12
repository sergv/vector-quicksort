----------------------------------------------------------------------------
-- |
-- Module      :  Data.Vector.Algorithms.Quicksort.Predefined.UIntParallelStrategiesMedian3IO
-- Copyright   :  (c) Sergey Vinokurov 2023
-- License     :  Apache-2.0 (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

module Data.Vector.Algorithms.Quicksort.Predefined.UIntParallelStrategiesMedian3IO
  ( sortUIntParallelStrategiesMedian3IO
  ) where

import Control.Monad.ST
import Data.Int
import Data.Vector.Unboxed qualified as U

import Data.Vector.Algorithms.Quicksort qualified as Quick
import Data.Vector.Algorithms.Quicksort.Fork
import Data.Vector.Algorithms.Quicksort.Median


{-# NOINLINE sortUIntParallelStrategiesMedian3IO #-}
sortUIntParallelStrategiesMedian3IO :: U.MVector RealWorld Int64 -> IO ()
sortUIntParallelStrategiesMedian3IO = Quick.sort ParStrategies (Median3 @Int64)

