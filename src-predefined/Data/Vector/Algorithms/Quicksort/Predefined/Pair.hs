-- |
-- Module:     Data.Vector.Algorithms.Quicksort.Predefined.Pair
-- Copyright:  (c) Sergey Vinokurov 2023
-- License:    Apache-2.0 (see LICENSE)
-- Maintainer: serg.foo@gmail.com

{-# LANGUAGE DerivingVia  #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Vector.Algorithms.Quicksort.Predefined.Pair
  ( TestPair(..)
  , toTuple
  ) where

import Data.Vector.Generic qualified as G
import Data.Vector.Generic.Mutable qualified as GM
import Data.Vector.Unboxed qualified as U

data TestPair a b = TestPair a b
  deriving (Show)

{-# INLINE toTuple #-}
toTuple :: TestPair a b -> (a, b)
toTuple (TestPair a b) = (a, b)

instance U.IsoUnbox (TestPair a b) (a, b) where
  {-# INLINE toURepr   #-}
  {-# INLINE fromURepr #-}
  toURepr          = toTuple
  fromURepr (a, b) = TestPair a b

newtype instance U.MVector s (TestPair a b) = MV_TestPair (U.MVector s (a, b))
newtype instance U.Vector    (TestPair a b) = V_TestPair  (U.Vector    (a, b))
deriving via (TestPair a b `U.As` (a, b)) instance (U.Unbox a, U.Unbox b) => GM.MVector U.MVector (TestPair a b)
deriving via (TestPair a b `U.As` (a, b)) instance (U.Unbox a, U.Unbox b) => G.Vector   U.Vector  (TestPair a b)
instance (U.Unbox a, U.Unbox b) => U.Unbox (TestPair a b)

instance Eq a => Eq (TestPair a b) where
  TestPair x _ == TestPair x' _ = x == x'

instance Ord a => Ord (TestPair a b) where
  TestPair x _ `compare` TestPair x' _ = x `compare` x'
