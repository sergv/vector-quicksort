-- |
-- Module:     Data.Vector.Algorithms.Quicksort.Predefined.AveragingMedian
-- Copyright:  (c) Sergey Vinokurov 2023
-- License:    Apache-2.0 (see LICENSE)
-- Maintainer: serg.foo@gmail.com

{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Vector.Algorithms.Quicksort.Predefined.AveragingMedian
  ( AveragingMedian(..)
  ) where

import Control.Monad.Primitive
import Data.Bits
import Data.Coerce
import Data.Int
import Data.Kind
import Data.Vector.Generic.Mutable qualified as GM

import Data.Vector.Algorithms.Quicksort.Median
import Data.Vector.Algorithms.Quicksort.Predefined.Pair

data AveragingMedian a = AveragingMedian

instance (PrimMonad m, s ~ PrimState m) => Median (AveragingMedian Int64) Int64 m s where
  {-# INLINE selectMedianOn #-}
  selectMedianOn
    :: forall (v :: Type -> Type -> Type) a.
       (GM.MVector v a, Coercible a Int64)
    => AveragingMedian Int64
    -> v s a
    -> m (MedianResult a)
  selectMedianOn _ !v = do
    let len :: Int
        !len = GM.length v
        pi0, pi1, pi2 :: Int
        !pi0  = 0
        !pi1  = halve len
        !pi2  = len - 1
    (!pv0 :: a) <- GM.unsafeRead v pi0
    !pv1        <- GM.unsafeRead v pi1
    !pv2        <- GM.unsafeRead v pi2
    pure $! Guess $ coerce $ (coerce pv0 :: Int64) + coerce pv1 + coerce pv2 `quot` 3

instance
  (PrimMonad m, s ~ PrimState m)
  =>
  Median (AveragingMedian (TestPair Int32 b)) (TestPair Int32 b) m s where
  {-# INLINE selectMedianOn #-}
  selectMedianOn
    :: forall (v :: Type -> Type -> Type) a.
       (GM.MVector v a, Coercible a (TestPair Int32 b))
    => AveragingMedian (TestPair Int32 b)
    -> v s a
    -> m (MedianResult a)
  selectMedianOn _ !v = do
    let len :: Int
        !len = GM.length v
        pi0, pi1, pi2 :: Int
        !pi0  = 0
        !pi1  = halve len
        !pi2  = len - 1
    !(TestPair (!pv0, pv0')) <-                 toTestPair <$> GM.unsafeRead v pi0
    !pv1                     <- fst . toTuple . toTestPair <$> GM.unsafeRead v pi1
    !pv2                     <- fst . toTuple . toTestPair <$> GM.unsafeRead v pi2
    pure $! Guess $ coerce $ TestPair (pv0 + pv1 + pv2 `quot` 3, pv0')
    where
      toTestPair :: a -> TestPair Int32 b
      toTestPair = coerce

{-# INLINE halve #-}
halve :: Int -> Int
halve x = x `unsafeShiftR` 1

