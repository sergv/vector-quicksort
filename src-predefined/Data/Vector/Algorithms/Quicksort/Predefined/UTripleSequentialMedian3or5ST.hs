-- |
-- Module:     Data.Vector.Algorithms.Quicksort.Predefined.UTripleSequentialMedian3or5ST
-- Copyright:  (c) Sergey Vinokurov 2023
-- License:    Apache-2.0 (see LICENSE)
-- Maintainer: serg.foo@gmail.com

module Data.Vector.Algorithms.Quicksort.Predefined.UTripleSequentialMedian3or5ST
  ( sortUTripleSequentialMedian3or5ST
  ) where

import Control.Monad.ST
import Data.Vector.Unboxed qualified as U

import Data.Vector.Algorithms.Quicksort.Parameterised

import Data.Vector.Algorithms.Quicksort.Predefined.SortTriple

import Data.Vector.Algorithms.Quicksort.Predefined.UTripleSequentialMedian3ST ()

{-# NOINLINE sortUTripleSequentialMedian3or5ST #-}
sortUTripleSequentialMedian3or5ST :: U.MVector s SortTriple -> ST s ()
sortUTripleSequentialMedian3or5ST = sortInplaceFM Sequential (Median3or5 @SortTriple)


