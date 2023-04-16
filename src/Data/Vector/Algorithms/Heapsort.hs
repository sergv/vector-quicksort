-- |
-- Module:     Data.Vector.Algorithms.Heapsort
-- Copyright:  (c) Sergey Vinokurov 2023
-- License:    Apache-2.0 (see LICENSE)
-- Maintainer: serg.foo@gmail.com

{-# LANGUAGE MagicHash        #-}
{-# LANGUAGE UnliftedNewtypes #-}

module Data.Vector.Algorithms.Heapsort
  ( heapSortOn
  ) where

import Control.Monad.Primitive
import Data.Bits
import Data.Coerce
import Data.Vector.Generic.Mutable qualified as GM
import GHC.Exts (Proxy#)

{-# INLINABLE shiftDownOn #-}
shiftDownOn
  :: forall m v a b. (PrimMonad m, GM.MVector v a, Coercible a b, Ord b)
  => Proxy# b
  -> v (PrimState m) a
  -> Int
  -> m ()
shiftDownOn _ !v = go
  where
    !end = GM.length v
    go !p
      | c1 < end
      = do
        let !c2 = c1 + 1
        c1Val <- GM.unsafeRead v c1
        (maxIdx, maxVal) <-
          if c2 < end
          then do
            c2Val <- GM.unsafeRead v c2
            pure $ if c2Val <@ c1Val then (c1, c1Val) else (c2, c2Val)
          else pure (c1, c1Val)
        pVal <- GM.unsafeRead v p
        if pVal <@ maxVal
        then do
          GM.unsafeWrite v p maxVal
          GM.unsafeWrite v maxIdx pVal
          go maxIdx
        else
          pure ()
      | otherwise
      = pure ()
      where
        !c1 = p * 2 + 1

    (<@) :: a -> a -> Bool
    (<@) = coerce ((<) :: b -> b -> Bool)

{-# INLINABLE heapifyOn #-}
heapifyOn
  :: (PrimMonad m, GM.MVector v a, Coercible a b, Ord b)
  => Proxy# b
  -> v (PrimState m) a
  -> m ()
heapifyOn p !v =
  go (GM.length v `unsafeShiftR` 1)
  where
    go 0 = shiftDownOn p v 0
    go n = shiftDownOn p v n *> go (n - 1)

{-# INLINABLE heapSortOn #-}
-- | O(N * log(N)) regular heapsort (with 2-way heap, whereas vector-algorithm's is 4-way).
-- Can be used as a standalone sort but main purpose is fallback sort for quicksort.
--
-- Depending on GHC may be good candidate for SPECIALIZE pragma.
heapSortOn
  :: (PrimMonad m, GM.MVector v a, Coercible a b, Ord b)
  => Proxy# b
  -> v (PrimState m) a
  -> m ()
heapSortOn p !v = do
  heapifyOn p v
  go (GM.length v)
  where
    go 0 = pure ()
    go n = do
      let !k = n - 1
      GM.unsafeSwap v 0 k
      shiftDownOn p (GM.unsafeSlice 0 k v) 0
      go k
