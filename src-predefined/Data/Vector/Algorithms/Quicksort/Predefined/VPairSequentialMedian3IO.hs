-- |
-- Module:     Data.Vector.Algorithms.Quicksort.Predefined.VPairSequentialMedian3IO
-- Copyright:  (c) Sergey Vinokurov 2023
-- License:    Apache-2.0 (see LICENSE)
-- Maintainer: serg.foo@gmail.com

{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Vector.Algorithms.Quicksort.Predefined.VPairSequentialMedian3IO
  ( sortVPairSequentialMedian3IO
  ) where

import Control.Monad.ST
import Data.Int
import Data.Vector qualified as V

import Data.Vector.Algorithms.Quicksort.Parameterised

import Data.Vector.Algorithms.Quicksort.Predefined.Pair

import Data.Vector.Algorithms.FixedSort
import Data.Vector.Algorithms.Heapsort

{-# SPECIALIZE heapSort    :: V.MVector RealWorld (TestPair Int32 Int32) -> IO ()        #-}
{-# SPECIALIZE bitonicSort :: Int -> V.MVector RealWorld (TestPair Int32 Int32) -> IO () #-}


{-# NOINLINE sortVPairSequentialMedian3IO #-}
sortVPairSequentialMedian3IO :: V.MVector RealWorld (TestPair Int32 Int32) -> IO ()
sortVPairSequentialMedian3IO = sortFM Sequential (Median3 @(TestPair Int32 Int32))
