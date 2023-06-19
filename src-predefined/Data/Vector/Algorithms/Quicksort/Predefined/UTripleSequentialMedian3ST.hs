-- |
-- Module:     Data.Vector.Algorithms.Quicksort.Predefined.UTripleSequentialMedian3ST
-- Copyright:  (c) Sergey Vinokurov 2023
-- License:    Apache-2.0 (see LICENSE)
-- Maintainer: serg.foo@gmail.com

{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Vector.Algorithms.Quicksort.Predefined.UTripleSequentialMedian3ST
  ( sortUTripleSequentialMedian3ST
  ) where

import Control.Monad.ST
import Data.Vector.Unboxed qualified as U

import Data.Vector.Algorithms.Quicksort.Parameterised

import Data.Vector.Algorithms.FixedSort
import Data.Vector.Algorithms.Heapsort

import Data.Vector.Algorithms.Quicksort.Predefined.SortTriple

{-# SPECIALIZE heapSort    :: U.MVector s SortTriple -> ST s ()        #-}
{-# SPECIALIZE bitonicSort :: Int -> U.MVector s SortTriple -> ST s () #-}

{-# NOINLINE sortUTripleSequentialMedian3ST #-}
sortUTripleSequentialMedian3ST :: U.MVector s SortTriple -> ST s ()
sortUTripleSequentialMedian3ST = sortInplaceFM Sequential (Median3 @SortTriple)


