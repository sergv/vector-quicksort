-- |
-- Module:     Data.Vector.Algorithms.Quicksort.Predefined.UIntSequentialMedian3IO
-- Copyright:  (c) Sergey Vinokurov 2023
-- License:    Apache-2.0 (see LICENSE)
-- Maintainer: serg.foo@gmail.com

{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Vector.Algorithms.Quicksort.Predefined.UIntSequentialMedian3IO
  ( sortUIntSequentialMedian3IO
  ) where

import Control.Monad.ST
import Data.Int
import Data.Vector.Unboxed qualified as U

import Data.Vector.Algorithms.Quicksort.Parameterised

import Data.Vector.Algorithms.FixedSort
import Data.Vector.Algorithms.Heapsort

{-# SPECIALIZE heapSort    :: U.MVector RealWorld Int64 -> IO ()        #-}
{-# SPECIALIZE bitonicSort :: Int -> U.MVector RealWorld Int64 -> IO () #-}

{-# NOINLINE sortUIntSequentialMedian3IO #-}
sortUIntSequentialMedian3IO :: U.MVector RealWorld Int64 -> IO ()
sortUIntSequentialMedian3IO = sortFM Sequential (Median3 @Int64)

