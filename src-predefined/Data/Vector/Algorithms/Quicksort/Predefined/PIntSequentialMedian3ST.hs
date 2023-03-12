----------------------------------------------------------------------------
-- |
-- Module      :  Data.Vector.Algorithms.Quicksort.Predefined.VIntSequentialMedian3ST
-- Copyright   :  (c) Sergey Vinokurov 2023
-- License     :  Apache-2.0 (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

module Data.Vector.Algorithms.Quicksort.Predefined.PIntSequentialMedian3ST
  ( sortPIntSequentialMedian3ST
  ) where

import Control.Monad.ST
import Data.Int
import Data.Vector.Primitive qualified as P

import Data.Vector.Algorithms.Quicksort qualified as Quick
import Data.Vector.Algorithms.Quicksort.Fork
import Data.Vector.Algorithms.Quicksort.Median


{-# NOINLINE sortPIntSequentialMedian3ST #-}
sortPIntSequentialMedian3ST :: P.MVector s Int64 -> ST s ()
sortPIntSequentialMedian3ST = Quick.sort Sequential (Median3 @Int64)


