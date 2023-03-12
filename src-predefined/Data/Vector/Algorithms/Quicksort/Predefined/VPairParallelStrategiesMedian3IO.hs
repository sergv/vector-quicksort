----------------------------------------------------------------------------
-- |
-- Module      :  Data.Vector.Algorithms.Quicksort.Predefined.VPairParallelStrategiesMedian3IO
-- Copyright   :  (c) Sergey Vinokurov 2023
-- License     :  Apache-2.0 (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

module Data.Vector.Algorithms.Quicksort.Predefined.VPairParallelStrategiesMedian3IO
  ( sortVPairParallelStrategiesMedian3IO
  ) where

import Control.Monad.ST
import Data.Int
import Data.Vector qualified as V

import Data.Vector.Algorithms.Quicksort qualified as Quick
import Data.Vector.Algorithms.Quicksort.Fork
import Data.Vector.Algorithms.Quicksort.Median


{-# NOINLINE sortVPairParallelStrategiesMedian3IO #-}
sortVPairParallelStrategiesMedian3IO :: V.MVector RealWorld (Int32, Int32) -> IO ()
sortVPairParallelStrategiesMedian3IO = Quick.sort ParStrategies (Median3 @(Int32, Int32))

