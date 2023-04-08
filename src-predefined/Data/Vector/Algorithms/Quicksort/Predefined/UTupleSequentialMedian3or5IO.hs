-- |
-- Module:     Data.Vector.Algorithms.Quicksort.Predefined.UTupleSequentialMedian3or5IO
-- Copyright:  (c) Sergey Vinokurov 2023
-- License:    Apache-2.0 (see LICENSE)
-- Maintainer: serg.foo@gmail.com

module Data.Vector.Algorithms.Quicksort.Predefined.UTupleSequentialMedian3or5IO
  ( sortUTupleSequentialMedian3or5IO
  ) where

import Control.Monad.ST
import Data.Int
import Data.Vector.Unboxed qualified as U

import Data.Vector.Algorithms.Quicksort.Parameterised

import Data.Vector.Algorithms.Quicksort.Predefined.UTupleSequentialMedian3IO ()

{-# NOINLINE sortUTupleSequentialMedian3or5IO #-}
sortUTupleSequentialMedian3or5IO :: U.MVector RealWorld (Int32, Int32) -> IO ()
sortUTupleSequentialMedian3or5IO = sortFM Sequential (Median3or5 @(Int32, Int32))


