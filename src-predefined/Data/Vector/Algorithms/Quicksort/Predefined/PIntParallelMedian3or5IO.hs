-- |
-- Module:     Data.Vector.Algorithms.Quicksort.Predefined.PIntParallelMedian3or5IO
-- Copyright:  (c) Sergey Vinokurov 2023
-- License:    Apache-2.0 (see LICENSE)
-- Maintainer: serg.foo@gmail.com

module Data.Vector.Algorithms.Quicksort.Predefined.PIntParallelMedian3or5IO
  ( sortPIntParallelMedian3or5IO
  ) where

import Control.Concurrent (getNumCapabilities)
import Control.Monad.ST
import Data.Int
import Data.Vector.Primitive qualified as P

import Data.Vector.Algorithms.Quicksort.Parameterised

import Data.Vector.Algorithms.Quicksort.Predefined.PIntSequentialMedian3IO ()

{-# NOINLINE sortPIntParallelMedian3or5IO #-}
sortPIntParallelMedian3or5IO :: P.MVector RealWorld Int64 -> IO ()
sortPIntParallelMedian3or5IO xs = do
  p <- mkParallel =<< getNumCapabilities
  sortInplaceFM p (Median3or5 @Int64) xs
  waitParallel p

