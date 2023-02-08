----------------------------------------------------------------------------
-- |
-- Module      :  Data.Vector.Algorithms.Quicksort.Median
-- Copyright   :  (c) Sergey Vinokurov 2023
-- License     :  Apache-2.0 (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

{-# LANGUAGE FunctionalDependencies #-}

module Data.Vector.Algorithms.Quicksort.Median
  ( Median(..)
  , Median3(..)
  , Median3or5(..)
  ) where

import Prelude hiding (last)

import Control.Monad.Primitive
import Data.Bits
import Data.Kind (Type)
import Data.Vector.Generic.Mutable qualified as GM

-- import Debug.Trace qualified
-- import Data.FuzzyMatch.SortKey
-- import Data.Int
-- import Data.Word

class Median a b | a -> b where
  selectMedian :: (PrimMonad m, GM.MVector v b, Ord b) => a -> v (PrimState m) b -> m b

data Median3 a = Median3

data Median3or5 a = Median3or5

-- to32 :: Integral a => a -> Int32
-- to32 = fromIntegral

{-# INLINE pick3 #-}
pick3 :: Ord a => a -> a -> a -> a
pick3 a b c =
  if b < a
  then
    -- ... b < a ...
    if c < a
    then
      if c < b
      then
        -- c < b < a
        b
      else
        -- b <= c < a
        c
    else
      --  b < a <= c
      a
  else
    -- ... a <= b ...
    if c < b
    then
      if c < a
      then
        -- c < a <= b
        a
      else
        -- a <= c <= b
        c
    else
      -- a <= b <= c
      b

{-# INLINE sort3 #-}
sort3 :: Ord a => a -> a -> a -> (a, a, a)
sort3 a b c =
  if b < a
  then
    -- ... b < a ...
    if c < a
    then
      if c < b
      then
        -- c < b < a
        (c, b, a)
      else
        -- b <= c < a
        (b, c, a)
    else
      --  b < a <= c
      (b, a, c)
  else
    -- ... a <= b ...
    if c < b
    then
      if c < a
      then
        -- c < a <= b
        (c, a, b)
      else
        -- a <= c <= b
        (a, c, b)
    else
      -- a <= b <= c
      (a, b, c)


instance Median (Median3 a) a where
  {-# INLINE selectMedian #-}
  selectMedian
    :: forall (m :: Type -> Type) (v :: Type -> Type -> Type).
       (PrimMonad m, GM.MVector v a, Ord a)
    => Median3 a
    -> v (PrimState m) a
    -> m a
  selectMedian _ v = do
    let len :: Int
        !len = GM.length v
        pi0, pi1, pi2 :: Int
        !pi0  = 0
        !pi1  = len `unsafeShiftR` 1
        !pi2  = last
        last :: Int
        !last = len - 1
    (pv0 :: a) <- GM.unsafeRead v pi0
    (pv1 :: a) <- GM.unsafeRead v pi1
    (pv2 :: a) <- GM.unsafeRead v pi2

    pure $! pick3 pv0 pv1 pv2


instance Median (Median3or5 a) a where
  {-# INLINE selectMedian #-}
  selectMedian
    :: forall (m :: Type -> Type) (v :: Type -> Type -> Type).
       (PrimMonad m, GM.MVector v a, Ord a)
    => Median3or5 a
    -> v (PrimState m) a
    -> m a
  selectMedian _ v = do
    let len :: Int
        !len = GM.length v
        pi0, pi1, pi2 :: Int
        !pi0  = 0
        !pi1  = len `unsafeShiftR` 1
        !pi2  = last
        last :: Int
        !last = len - 1
    (pv0 :: a) <- GM.unsafeRead v pi0
    (pv1 :: a) <- GM.unsafeRead v pi1
    (pv2 :: a) <- GM.unsafeRead v pi2

    if len < 1000
    then pure $! pick3 pv0 pv1 pv2
    else do
      let pi01, pi12 :: Int
          !pi01 = pi1 `unsafeShiftR` 1
          !pi12 = pi1 + pi01
    -- let !med = pick3 pv0 pv1 pv2

      (pv01 :: a) <- GM.unsafeRead v pi01
      (pv12 :: a) <- GM.unsafeRead v pi12

      let (!mn, !med, !mx) = sort3 pv0 pv1 pv2
          (!mn', !mx')
            | pv01 < pv12 = (pv01, pv12)
            | otherwise   = (pv12, pv01)

          !med'
            | mn' > mx  = mx
            | mx' < mn  = mn
            | otherwise = pick3 mn' med mx'

      -- Debug.Trace.trace ("pv0 = " ++ show (to32 pv0) ++ ", pv1 = " ++ show (to32 pv1) ++ ", pv2 = " ++ show (to32 pv2) ++ ", len = " ++ show len ++ ", pv01 = " ++ show (to32 pv01) ++ ", pv12 = " ++ show (to32 pv12) ++ ", med = " ++ show (to32 med) ++ ", med' = " ++ show (to32 med')) $
      pure $! med'




