-- |
-- Module:     Data.Vector.Algorithms.Quicksort
-- Copyright:  (c) Sergey Vinokurov 2023
-- License:    Apache-2.0 (see LICENSE)
-- Maintainer: serg.foo@gmail.com
--
-- This module provides reasonable default sorting algorithm with no parallelisation.
--
-- === Example
--
-- >>> import Control.Monad.ST (runST)
-- >>> import Data.Vector.Unboxed qualified as U
-- >>> :{
-- runST $ do
--   xs <- U.unsafeThaw $ U.fromList @Int [20, 19 .. 0]
--   sort xs
--   U.unsafeFreeze xs
-- :}
-- [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20]
--
-- === Performance considerations
-- For best performance it's recommended to keep a close eye on core
-- to make sure that this function doesn't take any class
-- dictionaries. If it does then performance will be very bad since
-- either comparisons will go via indirection, vector reads/writes,
-- monadic bind, or any combinatior of those will go through
-- dictionary indirection. This can be avoided either by compiling
-- with @-fspecialise-aggressively@ flag or by using via @SPECIALIZE@
-- pragmas, like so:
--
-- > -- Either use the flag to specialize everything, ...
-- > {-# OPTIONS_GHC -fspecialise-aggressively #-}
-- >
-- > -- ... or the pragmas for specific functions
-- > import Data.Vector.Algorithms.FixedSort
-- > import Data.Vector.Algorithms.Heapsort
-- > import Data.Vector.Algorithms.Quicksort
-- > import Data.Vector.Unboxed qualified as U
-- >
-- > -- If sorting in ST
-- > -- These are fallback sorts and their performance is important
-- > {-# SPECIALIZE heapSort    :: U.MVector s Int -> ST s ()        #-}
-- > {-# SPECIALIZE bitonicSort :: Int -> U.MVector s Int -> ST s () #-}
-- > -- Main sort entry point
-- > {-# SPECIALIZE sort        :: U.MVector s Int -> ST s ()        #-}
-- >
-- > -- If sorting in IO
-- > {-# SPECIALIZE heapSort    :: U.MVector RealWorld Int -> IO ()        #-}
-- > {-# SPECIALIZE bitonicSort :: Int -> U.MVector RealWorld Int -> IO () #-}
-- > {-# SPECIALIZE sort        :: U.MVector RealWorld Int -> IO ()        #-}

module Data.Vector.Algorithms.Quicksort
  ( sort
  ) where

import Prelude hiding (last, pi)

import Control.Monad.Primitive
import Data.Vector.Generic.Mutable qualified as GM

import Data.Vector.Algorithms.Quicksort.Parameterised

{-# INLINABLE sort #-}
-- | Good default sort.
--
-- This function takes generic mutable vectors so will work with any
-- vectors from the @vector@ package.
sort
  :: forall m a v.
     (PrimMonad m, Ord a, GM.MVector v a)
  => v (PrimState m) a
  -> m ()
sort = sortFM Sequential (Median3or5 @a)

