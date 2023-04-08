-- |
-- Module:     Data.Vector.Algorithms.Quicksort.Predefined.UPairSequentialAveragingMedianST
-- Copyright:  (c) Sergey Vinokurov 2023
-- License:    Apache-2.0 (see LICENSE)
-- Maintainer: serg.foo@gmail.com

module Data.Vector.Algorithms.Quicksort.Predefined.UPairSequentialAveragingMedianST
  ( sortUPairSequentialAveragingMedianST
  ) where

import Control.Monad.ST
import Data.Int
import Data.Vector.Unboxed qualified as U

import Data.Vector.Algorithms.Quicksort.Parameterised

import Data.Vector.Algorithms.Quicksort.Predefined.AveragingMedian
import Data.Vector.Algorithms.Quicksort.Predefined.Pair
import Data.Vector.Algorithms.Quicksort.Predefined.UPairSequentialMedian3ST ()

{-# NOINLINE sortUPairSequentialAveragingMedianST #-}
sortUPairSequentialAveragingMedianST :: U.MVector s (TestPair Int32 Int32) -> ST s ()
sortUPairSequentialAveragingMedianST = sortFM Sequential (AveragingMedian @(TestPair Int32 Int32))
