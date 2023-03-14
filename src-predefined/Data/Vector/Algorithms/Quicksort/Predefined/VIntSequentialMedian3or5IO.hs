----------------------------------------------------------------------------
-- |
-- Module      :  Data.Vector.Algorithms.Quicksort.Predefined.VIntSequentialMedian3or5IO
-- Copyright   :  (c) Sergey Vinokurov 2023
-- License     :  Apache-2.0 (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

module Data.Vector.Algorithms.Quicksort.Predefined.VIntSequentialMedian3or5IO
  ( sortVIntSequentialMedian3or5IO
  ) where

import Control.Monad.ST
import Data.Int
import Data.Vector qualified as V

import Data.Vector.Algorithms.Quicksort.Parameterised

import Data.Vector.Algorithms.Quicksort.Predefined.VIntSequentialMedian3IO ()

{-# NOINLINE sortVIntSequentialMedian3or5IO #-}
sortVIntSequentialMedian3or5IO :: V.MVector RealWorld Int64 -> IO ()
sortVIntSequentialMedian3or5IO = sortFM Sequential (Median3or5 @Int64)


