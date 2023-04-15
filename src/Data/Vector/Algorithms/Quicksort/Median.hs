-- |
-- Module:     Data.Vector.Algorithms.Quicksort.Median
-- Copyright:  (c) Sergey Vinokurov 2023
-- License:    Apache-2.0 (see LICENSE)
-- Maintainer: serg.foo@gmail.com

{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MagicHash              #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE UndecidableInstances   #-}

module Data.Vector.Algorithms.Quicksort.Median
  ( Median(..)
  , Median3(..)
  , Median3or5(..)
  , MedianResult(..)
  ) where

import Prelude hiding (last)

import Control.Monad.Primitive
import Data.Bits
import Data.Function
import Data.Kind (Type)
import Data.Vector.Generic.Mutable qualified as GM

-- | Median selection result.
data MedianResult a
  -- | Value that was located at specific index in the original array.
  = ExistingValue !a {-# UNPACK #-} !Int
  -- | Value that is a good guess for a real median but may not be
  -- present in the array (or we don't know where it's exactly).
  --
  -- Good example is to pick first, last, and middle element and
  -- average them, which restricts us to dealing with numeric values
  -- but may yield good results depending on distribution of values in
  -- the array to be sorted.
  | Guess !a

existingValue :: CmpFst a Int -> MedianResult a
existingValue (CmpFst (x, n)) = ExistingValue x n

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
-- Establish sortered order among 3 values.
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

newtype CmpFst a b = CmpFst { unCmpFst :: (a, b) }

instance Eq a => Eq (CmpFst a b) where
  (==) = (==) `on` fst . unCmpFst

instance Ord a => Ord (CmpFst a b) where
  compare = compare `on` fst . unCmpFst

{-# INLINE readAt #-}
readAt :: (PrimMonad m, GM.MVector v a) => v (PrimState m) a -> Int -> m (CmpFst a Int)
readAt xs n = (\x -> CmpFst (x, n)) <$> GM.unsafeRead xs n

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
        pi0, pi1, pi2 :: Int
        !pi0  = 0
        !pi1  = halve len
        !pi2  = len - 1
    !pv0 <- readAt v pi0
    !pv1 <- readAt v pi1
    !pv2 <- readAt v pi2
    pure $! existingValue $ pick3 pv0 pv1 pv2

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
        pi0, pi1, pi2 :: Int
        !pi0  = 0
        !pi1  = halve len
        !pi2  = len - 1
    !pv0 <- readAt v pi0
    !pv1 <- readAt v pi1
    !pv2 <- readAt v pi2

    -- If median of 3 has chances to be good enough
    if pv0 /= pv1 && pv1 /= pv2 && pv2 /= pv0
    then pure $! existingValue $ pick3 pv0 pv1 pv2
    else do
      let pi01, pi12 :: Int
          !pi01 = halve pi1
          !pi12 = pi1 + pi01

      !pv01 <- readAt v pi01
      !pv12 <- readAt v pi12

      let (!mn, !med, !mx) = sort3 pv0 pv1 pv2
          (!mn', !mx')
            | pv01 < pv12 = (pv01, pv12)
            | otherwise   = (pv12, pv01)

          !med'
            | mn' > mx  = mx
            | mx' < mn  = mn
            | otherwise = pick3 mn' med mx'

      pure $! existingValue med'

{-# INLINE halve #-}
halve :: Int -> Int
halve x = x `unsafeShiftR` 1
