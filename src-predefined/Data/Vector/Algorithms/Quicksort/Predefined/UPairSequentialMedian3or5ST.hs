----------------------------------------------------------------------------
-- |
-- Module      :  Data.Vector.Algorithms.Quicksort.Predefined.UPairSequentialMedian3or5ST
-- Copyright   :  (c) Sergey Vinokurov 2023
-- License     :  Apache-2.0 (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

module Data.Vector.Algorithms.Quicksort.Predefined.UPairSequentialMedian3or5ST
  ( sortUPairSequentialMedian3or5ST
  ) where

import Control.Monad.ST
import Data.Int
import Data.Vector.Unboxed qualified as U

import Data.Vector.Algorithms.Quicksort qualified as Quick
import Data.Vector.Algorithms.Quicksort.Fork
import Data.Vector.Algorithms.Quicksort.Median


{-# NOINLINE sortUPairSequentialMedian3or5ST #-}
sortUPairSequentialMedian3or5ST :: U.MVector s (Int32, Int32) -> ST s ()
sortUPairSequentialMedian3or5ST = Quick.sort Sequential (Median3or5 @(Int32, Int32))


