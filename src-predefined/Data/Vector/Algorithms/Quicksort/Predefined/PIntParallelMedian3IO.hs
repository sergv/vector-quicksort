----------------------------------------------------------------------------
-- |
-- Module      :  Data.Vector.Algorithms.Quicksort.Predefined.PIntParallelMedian3IO
-- Copyright   :  (c) Sergey Vinokurov 2023
-- License     :  Apache-2.0 (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

module Data.Vector.Algorithms.Quicksort.Predefined.PIntParallelMedian3IO
  ( sortPIntParallelMedian3IO
  ) where

import Control.Concurrent (getNumCapabilities)
import Control.Monad.ST
import Data.Int
import Data.Vector.Primitive qualified as P

import Data.Vector.Algorithms.Quicksort.Parameterised

import Data.Vector.Algorithms.Quicksort.Predefined.PIntSequentialMedian3IO ()

{-# NOINLINE sortPIntParallelMedian3IO #-}
sortPIntParallelMedian3IO :: P.MVector RealWorld Int64 -> IO ()
sortPIntParallelMedian3IO xs = do
  p <- mkParallel =<< getNumCapabilities
  sortFM p (Median3 @Int64) xs
  waitParallel p

