----------------------------------------------------------------------------
-- |
-- Module      :  Data.Vector.Algorithms.Quicksort.Predefined.VIntSequentialMedian3or5ST
-- Copyright   :  (c) Sergey Vinokurov 2023
-- License     :  Apache-2.0 (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

{-# OPTIONS_GHC -ddump-simpl -dsuppress-uniques -dsuppress-idinfo -dsuppress-module-prefixes -dsuppress-type-applications -dsuppress-coercions -dppr-cols200 -dsuppress-type-signatures -ddump-to-file #-}

module Data.Vector.Algorithms.Quicksort.Predefined.PIntSequentialMedian3or5IO
  ( sortPIntSequentialMedian3or5IO
  ) where

import Control.Monad.ST
import Data.Int
import Data.Vector.Primitive qualified as P

import Data.Vector.Algorithms.Quicksort.Parameterised

import Data.Vector.Algorithms.Quicksort.Predefined.PIntSequentialMedian3IO ()

{-# NOINLINE sortPIntSequentialMedian3or5IO #-}
sortPIntSequentialMedian3or5IO :: P.MVector RealWorld Int64 -> IO ()
sortPIntSequentialMedian3or5IO = sortFM Sequential (Median3or5 @Int64)

