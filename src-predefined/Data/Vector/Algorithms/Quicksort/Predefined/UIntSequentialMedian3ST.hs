-- |
-- Module:     Data.Vector.Algorithms.Quicksort.Predefined.UIntSequentialMedian3ST
-- Copyright:  (c) Sergey Vinokurov 2023
-- License:    Apache-2.0 (see LICENSE)
-- Maintainer: serg.foo@gmail.com

{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Vector.Algorithms.Quicksort.Predefined.UIntSequentialMedian3ST
  ( sortUIntSequentialMedian3ST
  ) where

import Control.Monad.ST
import Data.Int
import Data.Vector.Unboxed qualified as U

import Data.Vector.Algorithms.Quicksort.Parameterised

import Data.Vector.Algorithms.FixedSort
import Data.Vector.Algorithms.Heapsort

{-# SPECIALIZE heapSort    :: U.MVector s Int64 -> ST s ()        #-}
{-# SPECIALIZE bitonicSort :: Int -> U.MVector s Int64 -> ST s () #-}

{-# NOINLINE sortUIntSequentialMedian3ST #-}
sortUIntSequentialMedian3ST :: U.MVector s Int64 -> ST s ()
sortUIntSequentialMedian3ST = sortFM Sequential (Median3 @Int64)


