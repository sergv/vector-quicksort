-- |
-- Module:     Data.Vector.Algorithms.Quicksort.Predefined.VPairSequentialMedian3or5ST
-- Copyright:  (c) Sergey Vinokurov 2023
-- License:    Apache-2.0 (see LICENSE)
-- Maintainer: serg.foo@gmail.com

module Data.Vector.Algorithms.Quicksort.Predefined.VPairSequentialMedian3or5ST
  ( sortVPairSequentialMedian3or5ST
  ) where

import Control.Monad.ST
import Data.Int
import Data.Vector qualified as V

import Data.Vector.Algorithms.Quicksort.Parameterised

import Data.Vector.Algorithms.Quicksort.Predefined.Pair
import Data.Vector.Algorithms.Quicksort.Predefined.VPairSequentialMedian3ST ()

{-# NOINLINE sortVPairSequentialMedian3or5ST #-}
sortVPairSequentialMedian3or5ST :: V.MVector s (TestPair Int32 Int32) -> ST s ()
sortVPairSequentialMedian3or5ST = sortFM Sequential (Median3or5 @(TestPair Int32 Int32))


