-- |
-- Module:     Data.Vector.Algorithms.Quicksort.Predefined.VIntSequentialMedian3ST
-- Copyright:  (c) Sergey Vinokurov 2023
-- License:    Apache-2.0 (see LICENSE)
-- Maintainer: serg.foo@gmail.com

{-# LANGUAGE MagicHash #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Vector.Algorithms.Quicksort.Predefined.PIntSequentialMedian3ST
  ( sortPIntSequentialMedian3ST
  ) where

import Control.Monad.ST
import Data.Int
import Data.Vector.Primitive qualified as P
import GHC.Exts (Proxy#)

import Data.Vector.Algorithms.Quicksort.Parameterised

import Data.Vector.Algorithms.FixedSort
import Data.Vector.Algorithms.Heapsort

{-# SPECIALIZE heapSortOn    :: Proxy# Int64 -> P.MVector s Int64 -> ST s ()        #-}
{-# SPECIALIZE bitonicSortOn :: Proxy# Int64 -> Int -> P.MVector s Int64 -> ST s () #-}

{-# NOINLINE sortPIntSequentialMedian3ST #-}
sortPIntSequentialMedian3ST :: P.MVector s Int64 -> ST s ()
sortPIntSequentialMedian3ST = sortInplaceFM Sequential (Median3 @Int64)


