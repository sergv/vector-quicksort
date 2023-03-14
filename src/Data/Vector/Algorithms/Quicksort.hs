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

import Prelude hiding (last, pi)

import Control.Monad.Primitive
import Data.Vector.Generic.Mutable qualified as GM

import Data.Vector.Algorithms.Quicksort.Parameterised

{-# INLINABLE sort #-}
-- | Reasonable default sorting algorithm without parallelisation.
sort
  :: forall m a v.
     (PrimMonad m, Ord a, GM.MVector v a)
  => v (PrimState m) a
  -> m ()
sort = sortFM Sequential (Median3or5 @a)

