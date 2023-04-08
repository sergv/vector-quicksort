-- |
-- Module:     Data.Vector.Algorithms.Quicksort.Predefined.VPairSequentialMedian3ST
-- Copyright:  (c) Sergey Vinokurov 2023
-- License:    Apache-2.0 (see LICENSE)
-- Maintainer: serg.foo@gmail.com

{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Vector.Algorithms.Quicksort.Predefined.VPairSequentialMedian3ST
  ( sortVPairSequentialMedian3ST
  ) where

import Control.Monad.ST
import Data.Int
import Data.Vector qualified as V

import Data.Vector.Algorithms.Quicksort.Parameterised

import Data.Vector.Algorithms.Quicksort.Predefined.Pair

import Data.Vector.Algorithms.FixedSort
import Data.Vector.Algorithms.Heapsort

{-# SPECIALIZE heapSort    :: V.MVector s (TestPair Int32 Int32) -> ST s ()        #-}
{-# SPECIALIZE bitonicSort :: Int -> V.MVector s (TestPair Int32 Int32) -> ST s () #-}

{-# NOINLINE sortVPairSequentialMedian3ST #-}
sortVPairSequentialMedian3ST :: V.MVector s (TestPair Int32 Int32) -> ST s ()
sortVPairSequentialMedian3ST = sortFM Sequential (Median3 @(TestPair Int32 Int32))
