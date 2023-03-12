----------------------------------------------------------------------------
-- |
-- Module      :  Data.Vector.Algorithms.Quicksort.Predefined.UIntSequentialMedian3or5IO
-- Copyright   :  (c) Sergey Vinokurov 2023
-- License     :  Apache-2.0 (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

module Data.Vector.Algorithms.Quicksort.Predefined.UIntSequentialMedian3or5IO
  ( sortUIntSequentialMedian3or5IO
  ) where

import Control.Monad.ST
import Data.Int
import Data.Vector.Unboxed qualified as U

import Data.Vector.Algorithms.Quicksort qualified as Quick
import Data.Vector.Algorithms.Quicksort.Fork
import Data.Vector.Algorithms.Quicksort.Median


{-# NOINLINE sortUIntSequentialMedian3or5IO #-}
sortUIntSequentialMedian3or5IO :: U.MVector RealWorld Int64 -> IO ()
sortUIntSequentialMedian3or5IO = Quick.sort Sequential (Median3or5 @Int64)


