----------------------------------------------------------------------------
-- |
-- Module      :  Data.Vector.Algorithms.Quicksort
-- Copyright   :  (c) Sergey Vinokurov 2023
-- License     :  Apache-2.0 (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

module Data.Vector.Algorithms.Quicksort
  ( sort
  ) where

import Prelude hiding (last)

import Control.Monad.Primitive
import Data.Bits (countLeadingZeros)
import Data.Vector.Generic.Mutable qualified as GM

import Data.Vector.Algorithms.FixedSort
import Data.Vector.Algorithms.Heapsort
import Data.Vector.Algorithms.Quicksort.Fork
import Data.Vector.Algorithms.Quicksort.Median

{-# INLINABLE sort #-}
sort
  :: forall p med x m a v.
     (Fork p x m, Median med a, PrimMonad m, Ord a, GM.MVector v a)
  => p
  -> med
  -> v (PrimState m) a
  -> m ()
sort !p !med !vector = do
  !releaseToken <- startWork p
  qsortLoop 0 releaseToken threshold vector
  where
    threshold :: Int
    !threshold = binlog2 (GM.length vector)

    qsortLoop :: Int -> x -> Int -> v (PrimState m) a -> m ()
    qsortLoop !depth !releaseToken !cutoff !v
      | len < 17
      = bitonicSort len v *> endWork p releaseToken
      | cutoff == 0
      = heapSort v *> endWork p releaseToken
      | otherwise = do
        let last :: Int
            !last = len - 1
        !pv  <- selectMedian med v
        !pi' <- partitionTwoWays pv last v
        let !left    = GM.unsafeSlice 0 pi' v
            !right   = GM.unsafeSlice pi' (len - pi') v
            !cutoff' = cutoff - 1
            !depth'  = depth + 1
        fork
          p
          releaseToken
          depth
          (\token -> qsortLoop depth' token cutoff')
          (\token -> qsortLoop depth' token cutoff')
          left
          right
        -- qsortLoop cutoff' left
        -- qsortLoop cutoff' right
      where
        !len = GM.length v

{-# INLINE partitionTwoWays #-}
partitionTwoWays
  :: forall m a v. (PrimMonad m, Ord a, GM.MVector v a)
  => a -> Int -> v (PrimState m) a -> m Int
partitionTwoWays !pv !lastIdx !v =
  go 0 lastIdx
  where
    go :: Int -> Int -> m Int
    go !i !j = do
      (i', xi) <- goLT i
      (j', xj) <- goGT j
      if i' < j'
      then do
        GM.unsafeWrite v j' xi
        GM.unsafeWrite v i' xj
        go (i' + 1) (j' - 1)
      else
        pure i'
      where
        goLT !k = do
          x <- GM.unsafeRead v k
          if x < pv && k <= j
          then goLT (k + 1)
          else pure (k, x)
        goGT !k = do
          x <- GM.unsafeRead v k
          if x >= pv && k > i
          then goGT (k - 1)
          else pure (k, x)

{-# INLINE binlog2 #-}
binlog2 :: Int -> Int
binlog2 x = 63 - countLeadingZeros x


