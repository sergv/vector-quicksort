----------------------------------------------------------------------------
-- |
-- Module      :  Data.Vector.Algorithms.Quicksort.Predefined.UIntSequentialMedian3IO
-- Copyright   :  (c) Sergey Vinokurov 2023
-- License     :  Apache-2.0 (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

module Data.Vector.Algorithms.Quicksort.Predefined.UIntSequentialMedian3IO
  ( sortUIntSequentialMedian3IO
  ) where

import Control.Monad.ST
import Data.Int
import Data.Vector.Unboxed qualified as U

import Data.Vector.Algorithms.Quicksort qualified as Quick
import Data.Vector.Algorithms.Quicksort.Fork
import Data.Vector.Algorithms.Quicksort.Median


{-# NOINLINE sortUIntSequentialMedian3IO #-}
sortUIntSequentialMedian3IO :: U.MVector RealWorld Int64 -> IO ()
sortUIntSequentialMedian3IO = Quick.sort Sequential (Median3 @Int64)

