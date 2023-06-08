----------------------------------------------------------------------------
-- |
-- Module      :  Data.Vector.Algorithms.Quicksort.Predefined.BitonicIntST
-- Copyright   :  (c) Sergey Vinokurov 2023
-- License     :  Apache-2.0 (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

{-# LANGUAGE MagicHash #-}

module Data.Vector.Algorithms.Quicksort.Predefined.BitonicIntST (bitonicSortIntST) where

import Control.Monad.ST
import Data.Int
import Data.Vector.Primitive qualified as P
import Data.Vector.Primitive.Mutable qualified as PM
import GHC.Exts (proxy#)

import Data.Vector.Algorithms.FixedSort

{-# NOINLINE bitonicSortIntST #-}
bitonicSortIntST :: P.MVector s Int64 -> ST s ()
bitonicSortIntST xs = bitonicSortOn (proxy# @Int64) (PM.length xs) xs
