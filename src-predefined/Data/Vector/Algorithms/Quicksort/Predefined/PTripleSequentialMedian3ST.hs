-- |
-- Module:     Data.Vector.Algorithms.Quicksort.Predefined.PTripleSequentialMedian3ST
-- Copyright:  (c) Sergey Vinokurov 2023
-- License:    Apache-2.0 (see LICENSE)
-- Maintainer: serg.foo@gmail.com

{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Vector.Algorithms.Quicksort.Predefined.PTripleSequentialMedian3ST
  ( sortPTripleSequentialMedian3ST
  ) where

import Control.Monad.ST
import Data.Vector.Primitive qualified as P

import Data.Vector.Algorithms.Quicksort.Parameterised

import Data.Vector.Algorithms.FixedSort
import Data.Vector.Algorithms.Heapsort

import Data.Vector.Algorithms.Quicksort.Predefined.SortTriple

{-# SPECIALIZE heapSort    :: P.MVector s SortTriple -> ST s ()        #-}
{-# SPECIALIZE bitonicSort :: Int -> P.MVector s SortTriple -> ST s () #-}

{-# NOINLINE sortPTripleSequentialMedian3ST #-}
sortPTripleSequentialMedian3ST :: P.MVector s SortTriple -> ST s ()
sortPTripleSequentialMedian3ST = sortInplaceFM Sequential (Median3 @SortTriple)


