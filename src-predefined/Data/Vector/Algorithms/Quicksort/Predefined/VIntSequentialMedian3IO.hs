-- |
-- Module:     Data.Vector.Algorithms.Quicksort.Predefined.VIntSequentialMedian3IO
-- Copyright:  (c) Sergey Vinokurov 2023
-- License:    Apache-2.0 (see LICENSE)
-- Maintainer: serg.foo@gmail.com

{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Vector.Algorithms.Quicksort.Predefined.VIntSequentialMedian3IO
  ( sortVIntSequentialMedian3IO
  ) where

import Control.Monad.ST
import Data.Int
import Data.Vector qualified as V

import Data.Vector.Algorithms.Quicksort.Parameterised

import Data.Vector.Algorithms.FixedSort
import Data.Vector.Algorithms.Heapsort

{-# SPECIALIZE heapSort    :: V.MVector RealWorld Int64 -> IO ()        #-}
{-# SPECIALIZE bitonicSort :: Int -> V.MVector RealWorld Int64 -> IO () #-}

{-# NOINLINE sortVIntSequentialMedian3IO #-}
sortVIntSequentialMedian3IO :: V.MVector RealWorld Int64 -> IO ()
sortVIntSequentialMedian3IO = sortInplaceFM Sequential (Median3 @Int64)

