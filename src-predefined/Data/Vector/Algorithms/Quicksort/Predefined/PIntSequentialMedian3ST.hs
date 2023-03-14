----------------------------------------------------------------------------
-- |
-- Module      :  Data.Vector.Algorithms.Quicksort.Predefined.VIntSequentialMedian3ST
-- Copyright   :  (c) Sergey Vinokurov 2023
-- License     :  Apache-2.0 (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

{-# OPTIONS_GHC -ddump-simpl -dsuppress-uniques -dsuppress-idinfo -dsuppress-module-prefixes -dsuppress-type-applications -dsuppress-coercions -dppr-cols200 -dsuppress-type-signatures -ddump-to-file #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Vector.Algorithms.Quicksort.Predefined.PIntSequentialMedian3ST
  ( sortPIntSequentialMedian3ST
  ) where

import Control.Monad.ST
import Data.Int
import Data.Vector.Primitive qualified as P

import Data.Vector.Algorithms.Quicksort.Parameterised

import Data.Vector.Algorithms.FixedSort
import Data.Vector.Algorithms.Heapsort

{-# SPECIALIZE heapSort    :: P.MVector s Int64 -> ST s ()        #-}
{-# SPECIALIZE bitonicSort :: Int -> P.MVector s Int64 -> ST s () #-}

{-# NOINLINE sortPIntSequentialMedian3ST #-}
sortPIntSequentialMedian3ST :: P.MVector s Int64 -> ST s ()
sortPIntSequentialMedian3ST = sortFM Sequential (Median3 @Int64)


