-- |
-- Module:     Data.Vector.Algorithms.Quicksort.Predefined.VIntSequentialMedian3ST
-- Copyright:  (c) Sergey Vinokurov 2023
-- License:    Apache-2.0 (see LICENSE)
-- Maintainer: serg.foo@gmail.com

{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Vector.Algorithms.Quicksort.Predefined.PIntSequentialMedian3IO
  ( sortPIntSequentialMedian3IO
  ) where

import Control.Monad.ST
import Data.Int
import Data.Vector.Primitive qualified as P

import Data.Vector.Algorithms.FixedSort
import Data.Vector.Algorithms.Heapsort
import Data.Vector.Algorithms.Quicksort.Parameterised

{-# SPECIALIZE heapSort    :: P.MVector RealWorld Int64 -> IO ()        #-}
{-# SPECIALIZE bitonicSort :: Int -> P.MVector RealWorld Int64 -> IO () #-}

{-# NOINLINE sortPIntSequentialMedian3IO #-}
sortPIntSequentialMedian3IO :: P.MVector RealWorld Int64 -> IO ()
sortPIntSequentialMedian3IO = sortFM Sequential (Median3 @Int64)


