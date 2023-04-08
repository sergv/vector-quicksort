-- |
-- Module:     Data.Vector.Algorithms.Quicksort.Predefined.UPairSequentialMedian3ST
-- Copyright:  (c) Sergey Vinokurov 2023
-- License:    Apache-2.0 (see LICENSE)
-- Maintainer: serg.foo@gmail.com

{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Vector.Algorithms.Quicksort.Predefined.UPairSequentialMedian3ST
  ( sortUPairSequentialMedian3ST
  ) where

import Control.Monad.ST
import Data.Int
import Data.Vector.Unboxed qualified as U

import Data.Vector.Algorithms.Quicksort.Parameterised

import Data.Vector.Algorithms.Quicksort.Predefined.Pair

import Data.Vector.Algorithms.FixedSort
import Data.Vector.Algorithms.Heapsort

{-# SPECIALIZE heapSort    :: U.MVector s (TestPair Int32 Int32) -> ST s ()        #-}
{-# SPECIALIZE bitonicSort :: Int -> U.MVector s (TestPair Int32 Int32) -> ST s () #-}

{-# NOINLINE sortUPairSequentialMedian3ST #-}
sortUPairSequentialMedian3ST :: U.MVector s (TestPair Int32 Int32) -> ST s ()
sortUPairSequentialMedian3ST = sortFM Sequential (Median3 @(TestPair Int32 Int32))


