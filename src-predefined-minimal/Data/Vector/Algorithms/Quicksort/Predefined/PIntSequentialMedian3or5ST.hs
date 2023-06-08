-- |
-- Module:     Data.Vector.Algorithms.Quicksort.Predefined.VIntSequentialMedian3or5ST
-- Copyright:  (c) Sergey Vinokurov 2023
-- License:    Apache-2.0 (see LICENSE)
-- Maintainer: serg.foo@gmail.com

{-# OPTIONS_GHC -O2 -ddump-simpl -dsuppress-uniques -dsuppress-idinfo -dsuppress-module-prefixes -dsuppress-type-applications -dsuppress-coercions -dppr-cols200 -dsuppress-type-signatures -ddump-to-file #-}

module Data.Vector.Algorithms.Quicksort.Predefined.PIntSequentialMedian3or5ST
  ( sortPIntSequentialMedian3or5ST
  ) where

import Control.Monad.ST
import Data.Int
import Data.Proxy
import Data.Vector.Primitive qualified as P

import Data.Vector.Algorithms.Quicksort.Parameterised

import Data.Vector.Algorithms.Quicksort.Predefined.PIntSequentialMedian3ST ()

-- {-# SPECIALIZE sortInplaceFMOn
--   :: Proxy Int64
--   -> Sequential
--   -> Median3or5 Int64
--   -> P.MVector s Int64
--   -> ST s ()
--   #-}

{-# NOINLINE sortPIntSequentialMedian3or5ST #-}
sortPIntSequentialMedian3or5ST :: P.MVector s Int64 -> ST s ()
sortPIntSequentialMedian3or5ST = sortInplaceFM Sequential (Median3or5 @Int64)
