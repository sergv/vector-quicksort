----------------------------------------------------------------------------
-- |
-- Module      :  Data.Vector.Algorithms.Quicksort.Parameterised
-- Copyright   :  (c) Sergey Vinokurov 2023
-- License     :  Apache-2.0 (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

module Data.Vector.Algorithms.Quicksort.Parameterised
  ( sortFM
  -- * Reexports
  , module E
  ) where

import Prelude hiding (last, pi)

import Control.Monad
import Control.Monad.Primitive
import Data.Bits
import Data.Vector.Generic.Mutable qualified as GM

import Data.Vector.Algorithms.FixedSort
import Data.Vector.Algorithms.Heapsort
import Data.Vector.Algorithms.Quicksort.Fork2 as E
import Data.Vector.Algorithms.Quicksort.Median as E

{-# INLINABLE sortFM #-}
-- | Generic quicksort parameterised by median selection method and
-- parallelisation strategy.
sortFM
  :: forall p med x m a v.
     (Fork2 p x m, Median med a m (PrimState m), PrimMonad m, Ord a, GM.MVector v a)
  => p
  -> med
  -> v (PrimState m) a
  -> m ()
sortFM !p !med !vector = do
  !releaseToken <- startWork p
  qsortLoop 0 releaseToken vector
  where
    -- If we select bad median 4 times in a row then fall back to heapsort.
    !cutoffLen = GM.length vector `unsafeShiftL` 4

    qsortLoop :: Int -> x -> v (PrimState m) a -> m ()
    qsortLoop !depth !releaseToken !v
      | len < 17
      = bitonicSort len v *> endWork p releaseToken

      | len `unsafeShiftL` depth > cutoffLen
      = heapSort v *> endWork p releaseToken

      | otherwise = do
        let !last = len - 1
            v'    = GM.unsafeSlice 0 last v
        res <- selectMedian med v'

        (!pi', !pv) <- case res of
          Guess pv -> do
            (_, !pi') <- partitionTwoWays pv last v
            pure (pi', pv)

          ExistingValue pv pi -> do
            when (pi /= last) $ do
              GM.unsafeWrite v pi =<< GM.unsafeRead v last
              GM.unsafeWrite v last pv
            (!xi, !pi') <- partitionTwoWays pv (last - 1) v
            GM.unsafeWrite v pi' pv
            GM.unsafeWrite v last xi
            pure (pi', pv)

        !pi'' <- skipEq pv (pi' + 1) v

        let !left   = GM.unsafeSlice 0 pi' v
            !right  = GM.unsafeSlice pi'' (len - pi'') v
            !depth' = depth + 1
        fork
          p
          releaseToken
          depth
          (qsortLoop depth')
          (qsortLoop depth')
          left
          right
      where
        len = GM.length v

{-# INLINE partitionTwoWays #-}
partitionTwoWays
  :: (PrimMonad m, Ord a, GM.MVector v a)
  => a -> Int -> v (PrimState m) a -> m (a, Int)
partitionTwoWays !pv !lastIdx !v =
  go 0 lastIdx
  where
    go !i !j = do
      !(i', xi) <- goLT i
      !(j', xj) <- goGT j
      if i' < j'
      then do
        GM.unsafeWrite v j' xi
        GM.unsafeWrite v i' xj
        go (i' + 1) (j' - 1)
      else pure (xi, i')
      where
        goLT !k = do
          !x <- GM.unsafeRead v k
          if x < pv && k <= j
          then goLT (k + 1)
          else pure (k, x)
        goGT !k = do
          !x <- GM.unsafeRead v k
          if x >= pv && i < k
          then goGT (k - 1)
          else pure (k, x)

{-# INLINE skipEq #-}
-- Idetnify multiple pivots that are equal to the one we were partitioning with so that
-- whole run of equal pivots can be excluded from recursion.
skipEq :: (PrimMonad m, Eq a, GM.MVector v a) => a -> Int -> v (PrimState m) a -> m Int
skipEq !x !start !v = go start
  where
    !last = GM.length v
    go !k
      | k < last
      = do
        !y <- GM.unsafeRead v k
        if y == x
        then go (k + 1)
        else pure k
      | otherwise
      = pure k
