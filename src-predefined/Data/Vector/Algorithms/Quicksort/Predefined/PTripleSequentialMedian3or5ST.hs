-- |
-- Module:     Data.Vector.Algorithms.Quicksort.Predefined.PTripleSequentialMedian3or5ST
-- Copyright:  (c) Sergey Vinokurov 2023
-- License:    Apache-2.0 (see LICENSE)
-- Maintainer: serg.foo@gmail.com

module Data.Vector.Algorithms.Quicksort.Predefined.PTripleSequentialMedian3or5ST
  ( sortPTripleSequentialMedian3or5ST
  ) where

import Control.Monad.ST
import Data.Vector.Primitive qualified as P

import Data.Vector.Algorithms.Quicksort.Parameterised

import Data.Vector.Algorithms.Quicksort.Predefined.SortTriple

import Data.Vector.Algorithms.Quicksort.Predefined.PTripleSequentialMedian3ST ()

{-# NOINLINE sortPTripleSequentialMedian3or5ST #-}
sortPTripleSequentialMedian3or5ST :: P.MVector s SortTriple -> ST s ()
sortPTripleSequentialMedian3or5ST = sortInplaceFM Sequential (Median3or5 @SortTriple)


