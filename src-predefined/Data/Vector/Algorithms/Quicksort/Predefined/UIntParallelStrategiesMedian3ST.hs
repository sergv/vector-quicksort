----------------------------------------------------------------------------
-- |
-- Module      :  Data.Vector.Algorithms.Quicksort.Predefined.UIntParallelStrategiesMedian3ST
-- Copyright   :  (c) Sergey Vinokurov 2023
-- License     :  Apache-2.0 (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

module Data.Vector.Algorithms.Quicksort.Predefined.UIntParallelStrategiesMedian3ST
  ( sortUIntParallelStrategiesMedian3ST
  ) where

import Control.Monad.ST
import Data.Int
import Data.Vector.Unboxed qualified as U

import Data.Vector.Algorithms.Quicksort qualified as Quick
import Data.Vector.Algorithms.Quicksort.Fork
import Data.Vector.Algorithms.Quicksort.Median


{-# NOINLINE sortUIntParallelStrategiesMedian3ST #-}
sortUIntParallelStrategiesMedian3ST :: U.MVector s Int64 -> ST s ()
sortUIntParallelStrategiesMedian3ST = Quick.sort ParStrategies (Median3 @Int64)


