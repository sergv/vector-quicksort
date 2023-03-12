----------------------------------------------------------------------------
-- |
-- Module      :  Data.Vector.Algorithms.Quicksort.Predefined.UPairSequentialMedian3ST
-- Copyright   :  (c) Sergey Vinokurov 2023
-- License     :  Apache-2.0 (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

module Data.Vector.Algorithms.Quicksort.Predefined.UPairSequentialMedian3ST
  ( sortUPairSequentialMedian3ST
  ) where

import Control.Monad.ST
import Data.Int
import Data.Vector.Unboxed qualified as U

import Data.Vector.Algorithms.Quicksort qualified as Quick
import Data.Vector.Algorithms.Quicksort.Fork
import Data.Vector.Algorithms.Quicksort.Median


{-# NOINLINE sortUPairSequentialMedian3ST #-}
sortUPairSequentialMedian3ST :: U.MVector s (Int32, Int32) -> ST s ()
sortUPairSequentialMedian3ST = Quick.sort Sequential (Median3 @(Int32, Int32))


