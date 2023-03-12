----------------------------------------------------------------------------
-- |
-- Module      :  Data.Vector.Algorithms.Quicksort.Predefined.VPairSequentialMedian3ST
-- Copyright   :  (c) Sergey Vinokurov 2023
-- License     :  Apache-2.0 (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

module Data.Vector.Algorithms.Quicksort.Predefined.VPairSequentialMedian3ST
  ( sortVPairSequentialMedian3ST
  ) where

import Control.Monad.ST
import Data.Int
import Data.Vector qualified as V

import Data.Vector.Algorithms.Quicksort qualified as Quick
import Data.Vector.Algorithms.Quicksort.Fork
import Data.Vector.Algorithms.Quicksort.Median


{-# NOINLINE sortVPairSequentialMedian3ST #-}
sortVPairSequentialMedian3ST :: V.MVector s (Int32, Int32) -> ST s ()
sortVPairSequentialMedian3ST = Quick.sort Sequential (Median3 @(Int32, Int32))


