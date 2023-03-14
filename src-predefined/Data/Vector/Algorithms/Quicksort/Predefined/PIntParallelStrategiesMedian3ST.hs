----------------------------------------------------------------------------
-- |
-- Module      :  Data.Vector.Algorithms.Quicksort.Predefined.VIntParallelStrategiesMedian3ST
-- Copyright   :  (c) Sergey Vinokurov 2023
-- License     :  Apache-2.0 (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

{-# OPTIONS_GHC -ddump-simpl -dsuppress-uniques -dsuppress-idinfo -dsuppress-module-prefixes -dsuppress-type-applications -dsuppress-coercions -dppr-cols200 -dsuppress-type-signatures -ddump-to-file #-}

module Data.Vector.Algorithms.Quicksort.Predefined.PIntParallelStrategiesMedian3ST
  ( sortPIntParallelStrategiesMedian3ST
  ) where

import Control.Monad.ST
import Data.Int
import Data.Vector.Primitive qualified as P

import Data.Vector.Algorithms.Quicksort.Parameterised

import Data.Vector.Algorithms.Quicksort.Predefined.PIntSequentialMedian3ST ()

{-# NOINLINE sortPIntParallelStrategiesMedian3ST #-}
sortPIntParallelStrategiesMedian3ST :: P.MVector s Int64 -> ST s ()
sortPIntParallelStrategiesMedian3ST = sortFM ParStrategies (Median3 @Int64)
