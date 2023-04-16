-- |
-- Module:     Data.Vector.Algorithms.Quicksort.Predefined.VIntSequentialMedian3ST
-- Copyright:  (c) Sergey Vinokurov 2023
-- License:    Apache-2.0 (see LICENSE)
-- Maintainer: serg.foo@gmail.com

{-# LANGUAGE MagicHash #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Vector.Algorithms.Quicksort.Predefined.VIntSequentialMedian3ST
  ( sortVIntSequentialMedian3ST
  ) where

import Control.Monad.ST
import Data.Int
import Data.Vector qualified as V
import GHC.Exts (Proxy#)

import Data.Vector.Algorithms.Quicksort.Parameterised

import Data.Vector.Algorithms.FixedSort
import Data.Vector.Algorithms.Heapsort

{-# SPECIALIZE heapSortOn    :: Proxy# Int64 -> V.MVector s Int64 -> ST s ()        #-}
{-# SPECIALIZE bitonicSortOn :: Proxy# Int64 -> Int -> V.MVector s Int64 -> ST s () #-}

{-# NOINLINE sortVIntSequentialMedian3ST #-}
sortVIntSequentialMedian3ST :: V.MVector s Int64 -> ST s ()
sortVIntSequentialMedian3ST = sortInplaceFM Sequential (Median3 @Int64)


