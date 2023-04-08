-- |
-- Module:     Data.Vector.Algorithms.Quicksort.Predefined.VTupleSequentialMedian3IO
-- Copyright:  (c) Sergey Vinokurov 2023
-- License:    Apache-2.0 (see LICENSE)
-- Maintainer: serg.foo@gmail.com

{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Vector.Algorithms.Quicksort.Predefined.VTupleSequentialMedian3IO
  ( sortVTupleSequentialMedian3IO
  ) where

import Control.Monad.ST
import Data.Int
import Data.Vector qualified as V

import Data.Vector.Algorithms.Quicksort.Parameterised

import Data.Vector.Algorithms.FixedSort
import Data.Vector.Algorithms.Heapsort

{-# SPECIALIZE heapSort    :: V.MVector RealWorld (Int32, Int32) -> IO ()        #-}
{-# SPECIALIZE bitonicSort :: Int -> V.MVector RealWorld (Int32, Int32) -> IO () #-}


{-# NOINLINE sortVTupleSequentialMedian3IO #-}
sortVTupleSequentialMedian3IO :: V.MVector RealWorld (Int32, Int32) -> IO ()
sortVTupleSequentialMedian3IO = sortFM Sequential (Median3 @(Int32, Int32))
