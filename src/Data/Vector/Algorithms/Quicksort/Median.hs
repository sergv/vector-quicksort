-- |
-- Module:     Data.Vector.Algorithms.Quicksort.Median
-- Copyright:  (c) Sergey Vinokurov 2023
-- License:    Apache-2.0 (see LICENSE)
-- Maintainer: serg.foo@gmail.com

{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MagicHash              #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE UnboxedTuples          #-}

module Data.Vector.Algorithms.Quicksort.Median
  ( Median(..)
  , Median3(..)
  , Median3or5(..)
  , MedianResult(..)
  ) where

import Prelude hiding (last)

import Control.Monad.Primitive
import Data.Bits
import Data.Kind (Type)
import Data.Vector.Generic.Mutable qualified as GM
import GHC.Exts (Int(..), Int#)

-- | Median selection result.
data MedianResult a
  -- | Value that was located at specific index in the original array.
  = ExistingValue !a {-# UNPACK #-} !Int

existingValue :: (# a, Int# #) -> MedianResult a
existingValue (# !x, n #) = ExistingValue x (I# n)

-- | Median selection algorithm that, given a vector, should come up
-- with an elements that has good chances to be median (i.e to be
-- greater that half the elements and lower than the other remaining
-- half). The closer to the real median the selected element is, the
-- faster quicksort will run and the better parallelisation will be
-- achieved.
--
-- Instance can be declared for specific monad. This is useful if we want
-- to select median at random and need to thread random gen.
--
-- Parameter meaning;
-- - @a@ - the median parameter we're defining instance for
-- - @b@ - type of ellements this median selection method is applicable to
-- - @m@ - monad the median selection operates in
-- - @s@ - the same ‘index’ as in ‘ST s’ because vector to be sorted is parameterised and @m@ may need to mention it
class Median (a :: Type) (b :: Type) (m :: Type -> Type) (s :: Type) | a -> b, m -> s where
  -- | Come up with a median value of a given array
  selectMedian
    :: (GM.MVector v b, Ord b)
    => a     -- ^ Median algorithm than can carry extra info to be
             -- used during median selection (e.g. random generator)
    -> v s b -- ^ Array
    -> m (MedianResult b)

-- | Pick first, last, and the middle elements and find the one that's between the other two, e.g.
-- given elements @a@, @b@, and @c@ find @y@ among them that satisfies @x <= y <= z@.
data Median3 a = Median3

-- | Pick first, last, and the middle elements, if all of them are
-- distinct then return median of 3 like 'Median3' does, otherwise
-- take median of 5 from the already taken 3 and extra 2 elements at
-- @1/4@th and @3/4@th of array length.
data Median3or5 a = Median3or5

{-# INLINE pick3 #-}
-- Pick median among 3 values.
pick3 :: Ord a => a -> Int# -> a -> Int# -> a -> Int# -> (# a, Int# #)
pick3 !a ai !b bi !c ci =
  if b < a
  then
    -- ... b < a ...
    if c < a
    then
      if c < b
      then
        -- c < b < a
        (# b, bi #)
      else
        -- b <= c < a
        (# c, ci #)
    else
      --  b < a <= c
      (# a, ai #)
  else
    -- ... a <= b ...
    if c < b
    then
      if c < a
      then
        -- c < a <= b
        (# a, ai #)
      else
        -- a <= c <= b
        (# c, ci #)
    else
      -- a <= b <= c
      (# b, bi #)

{-# INLINE sort3 #-}
-- Establish sortered order among 3 values.
sort3 :: Ord a => a -> Int# -> a -> Int# -> a -> Int# -> (# a, Int#, a, Int#, a, Int# #)
sort3 !a ai !b bi !c ci =
  if b < a
  then
    -- ... b < a ...
    if c < a
    then
      if c < b
      then
        -- c < b < a
        (# c, ci, b, bi, a, ai #)
      else
        -- b <= c < a
        (# b, bi, c, ci, a, ai #)
    else
      --  b < a <= c
      (# b, bi, a, ai, c, ci #)
  else
    -- ... a <= b ...
    if c < b
    then
      if c < a
      then
        -- c < a <= b
        (# c, ci, a, ai, b, bi #)
      else
        -- a <= c <= b
        (# a, ai, c, ci, b, bi #)
    else
      -- a <= b <= c
      (# a, ai, b, bi, c, ci #)

instance (PrimMonad m, s ~ PrimState m) => Median (Median3 a) a m s where
  {-# INLINE selectMedian #-}
  selectMedian
    :: forall (v :: Type -> Type -> Type).
       (GM.MVector v a, Ord a)
    => Median3 a
    -> v s a
    -> m (MedianResult a)
  selectMedian _ !v = do
    let len :: Int
        !len = GM.length v
        pi0, pi1, pi2 :: Int#
        !(I# pi0)  = 0
        !(I# pi1)  = halve len
        !(I# pi2)  = len - 1
    !pv0 <- GM.unsafeRead v (I# pi0)
    !pv1 <- GM.unsafeRead v (I# pi1)
    !pv2 <- GM.unsafeRead v (I# pi2)
    pure $! existingValue (pick3 pv0 pi0 pv1 pi1 pv2 pi2)

instance (PrimMonad m, s ~ PrimState m) => Median (Median3or5 a) a m s where
  {-# INLINE selectMedian #-}
  selectMedian
    :: forall (v :: Type -> Type -> Type).
       (GM.MVector v a, Ord a)
    => Median3or5 a
    -> v s a
    -> m (MedianResult a)
  selectMedian _ !v = do
    let len :: Int
        !len = GM.length v
        pi0, pi1, pi2 :: Int#
        !(I# pi0)  = 0
        !(I# pi1)  = halve len
        !(I# pi2)  = len - 1
    !pv0 <- GM.unsafeRead v (I# pi0)
    !pv1 <- GM.unsafeRead v (I# pi1)
    !pv2 <- GM.unsafeRead v (I# pi2)

    -- If median of 3 has chances to be good enough
    if pv0 /= pv1 && pv1 /= pv2 && pv2 /= pv0
    then pure $! existingValue (pick3 pv0 pi0 pv1 pi1 pv2 pi2)
    else do
      let pi01, pi12 :: Int#
          !(I# pi01) = halve (I# pi1)
          !(I# pi12) = I# pi1 + I# pi01

      !pv01 <- GM.unsafeRead v (I# pi01)
      !pv12 <- GM.unsafeRead v (I# pi12)

      let !(# !mn, !mni, !med, !medi, !mx, !mxi #) = sort3 pv0 pi0 pv1 pi1 pv2 pi2
          !(# !mn', !mni', !mx', !mxi' #)
            | pv01 < pv12 = (# pv01, pi01, pv12, pi12 #)
            | otherwise   = (# pv12, pi12, pv01, pi01 #)

          !med'@(# !_, !_ #)
            | mn' > mx  = (# mx, mxi #)
            | mx' < mn  = (# mn, mni #)
            | otherwise = pick3 mn' mni' med medi mx' mxi'

      pure $! existingValue med'

{-# INLINE halve #-}
halve :: Int -> Int
halve x = x `unsafeShiftR` 1
