----------------------------------------------------------------------------
-- |
-- Module      :  Data.Vector.Algorithms.Quicksort.Predefined.VTupleSequentialMedian3or5IO
-- Copyright   :  (c) Sergey Vinokurov 2023
-- License     :  Apache-2.0 (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

module Data.Vector.Algorithms.Quicksort.Predefined.VTupleSequentialMedian3or5IO
  ( sortVTupleSequentialMedian3or5IO
  ) where

import Control.Monad.ST
import Data.Int
import Data.Vector qualified as V

import Data.Vector.Algorithms.Quicksort.Parameterised

import Data.Vector.Algorithms.Quicksort.Predefined.VTupleSequentialMedian3IO ()

{-# NOINLINE sortVTupleSequentialMedian3or5IO #-}
sortVTupleSequentialMedian3or5IO :: V.MVector RealWorld (Int32, Int32) -> IO ()
sortVTupleSequentialMedian3or5IO = sortFM Sequential (Median3or5 @(Int32, Int32))
