-- |
-- Module:     Data.Vector.Algorithms.Quicksort.Predefined.VIntSequentialMedian3ST
-- Copyright:  (c) Sergey Vinokurov 2023
-- License:    Apache-2.0 (see LICENSE)
-- Maintainer: serg.foo@gmail.com

module Data.Vector.Algorithms.Quicksort.Predefined.PIntSequentialAveragingMedianST
  ( sortPIntSequentialAveragingMedianST
  ) where

import Control.Monad.ST
import Data.Int
import Data.Vector.Primitive qualified as P

import Data.Vector.Algorithms.Quicksort.Parameterised

import Data.Vector.Algorithms.Quicksort.Predefined.AveragingMedian
import Data.Vector.Algorithms.Quicksort.Predefined.PIntSequentialMedian3ST ()

{-# NOINLINE sortPIntSequentialAveragingMedianST #-}
sortPIntSequentialAveragingMedianST :: P.MVector s Int64 -> ST s ()
sortPIntSequentialAveragingMedianST = sortInplaceFM Sequential (AveragingMedian @Int64)


