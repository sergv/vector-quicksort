-- |
-- Module:     Data.Vector.Algorithms.Quicksort.Predefined.VTupleSequentialMedian3ST
-- Copyright:  (c) Sergey Vinokurov 2023
-- License:    Apache-2.0 (see LICENSE)
-- Maintainer: serg.foo@gmail.com

{-# LANGUAGE MagicHash #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Vector.Algorithms.Quicksort.Predefined.VTupleSequentialMedian3ST
  ( sortVTupleSequentialMedian3ST
  ) where

import Control.Monad.ST
import Data.Int
import Data.Vector qualified as V
import GHC.Exts (Proxy#)

import Data.Vector.Algorithms.Quicksort.Parameterised

import Data.Vector.Algorithms.FixedSort
import Data.Vector.Algorithms.Heapsort

{-# SPECIALIZE heapSortOn    :: Proxy# (Int32, Int32) -> V.MVector s (Int32, Int32) -> ST s ()        #-}
{-# SPECIALIZE bitonicSortOn :: Proxy# (Int32, Int32) -> Int -> V.MVector s (Int32, Int32) -> ST s () #-}

{-# NOINLINE sortVTupleSequentialMedian3ST #-}
sortVTupleSequentialMedian3ST :: V.MVector s (Int32, Int32) -> ST s ()
sortVTupleSequentialMedian3ST = sortInplaceFM Sequential (Median3 @(Int32, Int32))
