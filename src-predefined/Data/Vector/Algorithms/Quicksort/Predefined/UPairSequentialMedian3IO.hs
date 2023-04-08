-- |
-- Module:     Data.Vector.Algorithms.Quicksort.Predefined.UPairSequentialMedian3IO
-- Copyright:  (c) Sergey Vinokurov 2023
-- License:    Apache-2.0 (see LICENSE)
-- Maintainer: serg.foo@gmail.com

{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Vector.Algorithms.Quicksort.Predefined.UPairSequentialMedian3IO
  ( sortUPairSequentialMedian3IO
  ) where

import Control.Monad.ST
import Data.Int
import Data.Vector.Unboxed qualified as U

import Data.Vector.Algorithms.Quicksort.Parameterised

import Data.Vector.Algorithms.Quicksort.Predefined.Pair

import Data.Vector.Algorithms.FixedSort
import Data.Vector.Algorithms.Heapsort

{-# SPECIALIZE heapSort    :: U.MVector RealWorld (TestPair Int32 Int32) -> IO ()        #-}
{-# SPECIALIZE bitonicSort :: Int -> U.MVector RealWorld (TestPair Int32 Int32) -> IO () #-}

{-# NOINLINE sortUPairSequentialMedian3IO #-}
sortUPairSequentialMedian3IO :: U.MVector RealWorld (TestPair Int32 Int32) -> IO ()
sortUPairSequentialMedian3IO = sortFM Sequential (Median3 @(TestPair Int32 Int32))
