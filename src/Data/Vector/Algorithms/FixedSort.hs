----------------------------------------------------------------------------
-- |
-- Module      :  Data.Vector.Algorithms.FixedSort
-- Copyright   :  (c) Sergey Vinokurov 2023
-- License     :  Apache-2.0 (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

module Data.Vector.Algorithms.FixedSort
  ( sort3
  , sort4
  , bitonicSort
  ) where

import Control.Monad hiding (forM)
import Control.Monad.Primitive
import Data.Vector.Generic.Mutable qualified as GM

{-# INLINABLE sort3 #-}
-- | Sorts the elements at the three given indices. The indices are assumed
-- to be given from lowest to highest, so if 'l < m < u' then
-- 'sort3ByIndex cmp a m l u' essentially sorts the median of three into the
-- lowest position in the array.
sort3
  :: (PrimMonad m, GM.MVector v a, Ord a)
  => v (PrimState m) a -> m ()
sort3 !xs = do
  x0 <- GM.unsafeRead xs 0
  x1 <- GM.unsafeRead xs 1
  x2 <- GM.unsafeRead xs 2
  if x1 < x0
  then
    if x2 < x0
    then
      if x2 < x1
      then do
        GM.unsafeWrite xs 0 x2
        GM.unsafeWrite xs 2 x0
      else do
         GM.unsafeWrite xs 0 x1
         GM.unsafeWrite xs 1 x2
         GM.unsafeWrite xs 2 x0
    else do
      GM.unsafeWrite xs 0 x1
      GM.unsafeWrite xs 1 x0
  else
    if x2 < x1
    then
      if x2 < x0
      then do
        GM.unsafeWrite xs 0 x2
        GM.unsafeWrite xs 1 x0
        GM.unsafeWrite xs 2 x1
      else do
        GM.unsafeWrite xs 1 x2
        GM.unsafeWrite xs 2 x1
    else
      pure ()

{-# INLINABLE sort4 #-}
-- | Sorts the elements at the four given indices. Like the 2 and 3 element
-- versions, this assumes that the indices are given in increasing order, so
-- it can be used to sort medians into particular positions and so on.
sort4
  :: (PrimMonad m, GM.MVector v a, Ord a)
  => v (PrimState m) a -> m ()
sort4 !xs = do
  x0 <- GM.unsafeRead xs 0
  x1 <- GM.unsafeRead xs 1
  x2 <- GM.unsafeRead xs 2
  x3 <- GM.unsafeRead xs 3
  if x1 < x0
  then
    if x2 < x0
    then
      if x2 < x1
      then
        if x3 < x1
        then
          if x3 < x2
          then do
            GM.unsafeWrite xs 0 x3
            GM.unsafeWrite xs 1 x2
            GM.unsafeWrite xs 2 x1
            GM.unsafeWrite xs 3 x0
          else do
            GM.unsafeWrite xs 0 x2
            GM.unsafeWrite xs 1 x3
            GM.unsafeWrite xs 2 x1
            GM.unsafeWrite xs 3 x0
        else
          if x3 < x0
          then do
            GM.unsafeWrite xs 0 x2
            GM.unsafeWrite xs 1 x1
            GM.unsafeWrite xs 2 x3
            GM.unsafeWrite xs 3 x0
          else do
            GM.unsafeWrite xs 0 x2
            GM.unsafeWrite xs 1 x1
            GM.unsafeWrite xs 2 x0
            GM.unsafeWrite xs 3 x3
      else
        if x3 < x2
        then
          if x3 < x1
          then do
            GM.unsafeWrite xs 0 x3
            GM.unsafeWrite xs 1 x1
            GM.unsafeWrite xs 2 x2
            GM.unsafeWrite xs 3 x0
          else do
            GM.unsafeWrite xs 0 x1
            GM.unsafeWrite xs 1 x3
            GM.unsafeWrite xs 2 x2
            GM.unsafeWrite xs 3 x0
        else
          if x3 < x0
          then do
            GM.unsafeWrite xs 0 x1
            GM.unsafeWrite xs 1 x2
            GM.unsafeWrite xs 2 x3
            GM.unsafeWrite xs 3 x0
          else do
            GM.unsafeWrite xs 0 x1
            GM.unsafeWrite xs 1 x2
            GM.unsafeWrite xs 2 x0
            -- GM.unsafeWrite xs 3 x3
    else
      if x3 < x0
      then
        if x3 < x1
        then do
          GM.unsafeWrite xs 0 x3
          -- GM.unsafeWrite xs 1 x1
          GM.unsafeWrite xs 2 x0
          GM.unsafeWrite xs 3 x2
        else do
          GM.unsafeWrite xs 0 x1
          GM.unsafeWrite xs 1 x3
          GM.unsafeWrite xs 2 x0
          GM.unsafeWrite xs 3 x2
      else
        if x3 < x2
        then do
          GM.unsafeWrite xs 0 x1
          GM.unsafeWrite xs 1 x0
          GM.unsafeWrite xs 2 x3
          GM.unsafeWrite xs 3 x2
        else do
          GM.unsafeWrite xs 0 x1
          GM.unsafeWrite xs 1 x0
          -- GM.unsafeWrite xs 2 x2
          -- GM.unsafeWrite xs 3 x3
  else
    if x2 < x1
    then
      if x2 < x0
      then
        if x3 < x0
        then
          if x3 < x2
          then do
            GM.unsafeWrite xs 0 x3
            GM.unsafeWrite xs 1 x2
            GM.unsafeWrite xs 2 x0
            GM.unsafeWrite xs 3 x1
          else do
            GM.unsafeWrite xs 0 x2
            GM.unsafeWrite xs 1 x3
            GM.unsafeWrite xs 2 x0
            GM.unsafeWrite xs 3 x1
        else
          if x3 < x1
          then do
            GM.unsafeWrite xs 0 x2
            GM.unsafeWrite xs 1 x0
            GM.unsafeWrite xs 2 x3
            GM.unsafeWrite xs 3 x1
          else do
            GM.unsafeWrite xs 0 x2
            GM.unsafeWrite xs 1 x0
            GM.unsafeWrite xs 2 x1
            -- GM.unsafeWrite xs 3 x3
      else
        if x3 < x2
        then
          if x3 < x0
          then do
            GM.unsafeWrite xs 0 x3
            GM.unsafeWrite xs 1 x0
            -- GM.unsafeWrite xs 2 x2
            GM.unsafeWrite xs 3 x1
          else do
            -- GM.unsafeWrite xs 0 x0
            GM.unsafeWrite xs 1 x3
            -- GM.unsafeWrite xs 2 x2
            GM.unsafeWrite xs 3 x1
        else
          if x3 < x1
          then do
            -- GM.unsafeWrite xs 0 x0
            GM.unsafeWrite xs 1 x2
            GM.unsafeWrite xs 2 x3
            GM.unsafeWrite xs 3 x1
          else do
            -- GM.unsafeWrite xs 0 x0
            GM.unsafeWrite xs 1 x2
            GM.unsafeWrite xs 2 x1
            -- GM.unsafeWrite xs 3 x3
    else
      if x3 < x1
      then
        if x3 < x0
        then do
          GM.unsafeWrite xs 0 x3
          GM.unsafeWrite xs 1 x0
          GM.unsafeWrite xs 2 x1
          GM.unsafeWrite xs 3 x2
        else do
          -- GM.unsafeWrite xs 0 x0
          GM.unsafeWrite xs 1 x3
          GM.unsafeWrite xs 2 x1
          GM.unsafeWrite xs 3 x2
      else
        if x3 < x2
        then do
          -- GM.unsafeWrite xs 0 x0
          -- GM.unsafeWrite xs 1 x1
          GM.unsafeWrite xs 2 x3
          GM.unsafeWrite xs 3 x2
        else do
          -- GM.unsafeWrite xs 0 x0
          -- GM.unsafeWrite xs 1 x1
          -- GM.unsafeWrite xs 2 x2
          -- GM.unsafeWrite xs 3 x3
          pure ()

{-# INLINABLE bitonicSort #-}
bitonicSort :: forall m v a. (PrimMonad m, Ord a, GM.MVector v a) => Int -> v (PrimState m) a -> m ()
bitonicSort !n !v = do
  case n of
    2  ->
      swap 0 1
    3  ->
      -- swap 1 2
      -- swap 0 2
      -- swap 0 1
      sort3 v
    4  ->
      -- swap 0 1
      -- swap 2 3
      -- swap 0 2
      -- swap 1 3
      -- swap 1 2
      sort4 v
    5  -> do
      swap 0 1
      swap 3 4
      swap 2 4
      swap 2 3
      swap 1 4
      swap 0 3
      swap 0 2
      swap 1 3
      swap 1 2
    6  -> do
      swap 1 2
      swap 4 5
      swap 0 2
      swap 3 5
      swap 0 1
      swap 3 4
      swap 2 5
      swap 0 3
      swap 1 4
      swap 2 4
      swap 1 3
      swap 2 3
    7  -> do
      swap 1 2
      swap 3 4
      swap 5 6
      swap 0 2
      swap 3 5
      swap 4 6
      swap 0 1
      swap 4 5
      swap 2 6
      swap 0 4
      swap 1 5
      swap 0 3
      swap 2 5
      swap 1 3
      swap 2 4
      swap 2 3
    8  -> do
      swap 0 1
      swap 2 3
      swap 4 5
      swap 6 7
      swap 0 2
      swap 1 3
      swap 4 6
      swap 5 7
      swap 1 2
      swap 5 6
      swap 0 4
      swap 3 7
      swap 1 5
      swap 2 6
      swap 1 4
      swap 3 6
      swap 2 4
      swap 3 5
      swap 3 4
    9  -> do
      swap 0 1
      swap 3 4
      swap 6 7
      swap 1 2
      swap 4 5
      swap 7 8
      swap 0 1
      swap 3 4
      swap 6 7
      swap 2 5
      swap 0 3
      swap 1 4
      swap 5 8
      swap 3 6
      swap 4 7
      swap 2 5
      swap 0 3
      swap 1 4
      swap 5 7
      swap 2 6
      swap 1 3
      swap 4 6
      swap 2 4
      swap 5 6
      swap 2 3
    10 -> do
      swap 4 9
      swap 3 8
      swap 2 7
      swap 1 6
      swap 0 5
      swap 1 4
      swap 6 9
      swap 0 3
      swap 5 8
      swap 0 2
      swap 3 6
      swap 7 9
      swap 0 1
      swap 2 4
      swap 5 7
      swap 8 9
      swap 1 2
      swap 4 6
      swap 7 8
      swap 3 5
      swap 2 5
      swap 6 8
      swap 1 3
      swap 4 7
      swap 2 3
      swap 6 7
      swap 3 4
      swap 5 6
      swap 4 5
    11 -> do
      swap 0 1
      swap 2 3
      swap 4 5
      swap 6 7
      swap 8 9
      swap 1 3
      swap 5 7
      swap 0 2
      swap 4 6
      swap 8 10
      swap 1 2
      swap 5 6
      swap 9 10
      swap 0 4
      swap 3 7
      swap 1 5
      swap 6 10
      swap 4 8
      swap 5 9
      swap 2 6
      swap 0 4
      swap 3 8
      swap 1 5
      swap 6 10
      swap 2 3
      swap 8 9
      swap 1 4
      swap 7 10
      swap 3 5
      swap 6 8
      swap 2 4
      swap 7 9
      swap 5 6
      swap 3 4
      swap 7 8
    12 -> do
      swap 0 1
      swap 2 3
      swap 4 5
      swap 6 7
      swap 8 9
      swap 10 11
      swap 1 3
      swap 5 7
      swap 9 11
      swap 0 2
      swap 4 6
      swap 8 10
      swap 1 2
      swap 5 6
      swap 9 10
      swap 0 4
      swap 7 11
      swap 1 5
      swap 6 10
      swap 3 7
      swap 4 8
      swap 5 9
      swap 2 6
      swap 0 4
      swap 7 11
      swap 3 8
      swap 1 5
      swap 6 10
      swap 2 3
      swap 8 9
      swap 1 4
      swap 7 10
      swap 3 5
      swap 6 8
      swap 2 4
      swap 7 9
      swap 5 6
      swap 3 4
      swap 7 8
    13 -> do
      swap 1 7
      swap 9 11
      swap 3 4
      swap 5 8
      swap 0 12
      swap 2 6
      swap 0 1
      swap 2 3
      swap 4 6
      swap 8 11
      swap 7 12
      swap 5 9
      swap 0 2
      swap 3 7
      swap 10 11
      swap 1 4
      swap 6 12
      swap 7 8
      swap 11 12
      swap 4 9
      swap 6 10
      swap 3 4
      swap 5 6
      swap 8 9
      swap 10 11
      swap 1 7
      swap 2 6
      swap 9 11
      swap 1 3
      swap 4 7
      swap 8 10
      swap 0 5
      swap 2 5
      swap 6 8
      swap 9 10
      swap 1 2
      swap 3 5
      swap 7 8
      swap 4 6
      swap 2 3
      swap 4 5
      swap 6 7
      swap 8 9
      swap 3 4
      swap 5 6
    14 -> do
      swap 0 1
      swap 2 3
      swap 4 5
      swap 6 7
      swap 8 9
      swap 10 11
      swap 12 13
      swap 0 2
      swap 4 6
      swap 8 10
      swap 1 3
      swap 5 7
      swap 9 11
      swap 0 4
      swap 8 12
      swap 1 5
      swap 9 13
      swap 2 6
      swap 3 7
      swap 0 8
      swap 1 9
      swap 2 10
      swap 3 11
      swap 4 12
      swap 5 13
      swap 5 10
      swap 6 9
      swap 3 12
      swap 7 11
      swap 1 2
      swap 4 8
      swap 1 4
      swap 7 13
      swap 2 8
      swap 5 6
      swap 9 10
      swap 2 4
      swap 11 13
      swap 3 8
      swap 7 12
      swap 6 8
      swap 10 12
      swap 3 5
      swap 7 9
      swap 3 4
      swap 5 6
      swap 7 8
      swap 9 10
      swap 11 12
      swap 6 7
      swap 8 9
    15 -> do
      swap 0 1
      swap 2 3
      swap 4 5
      swap 6 7
      swap 8 9
      swap 10 11
      swap 12 13
      swap 0 2
      swap 4 6
      swap 8 10
      swap 12 14
      swap 1 3
      swap 5 7
      swap 9 11
      swap 0 4
      swap 8 12
      swap 1 5
      swap 9 13
      swap 2 6
      swap 10 14
      swap 3 7
      swap 0 8
      swap 1 9
      swap 2 10
      swap 3 11
      swap 4 12
      swap 5 13
      swap 6 14
      swap 5 10
      swap 6 9
      swap 3 12
      swap 13 14
      swap 7 11
      swap 1 2
      swap 4 8
      swap 1 4
      swap 7 13
      swap 2 8
      swap 11 14
      swap 5 6
      swap 9 10
      swap 2 4
      swap 11 13
      swap 3 8
      swap 7 12
      swap 6 8
      swap 10 12
      swap 3 5
      swap 7 9
      swap 3 4
      swap 5 6
      swap 7 8
      swap 9 10
      swap 11 12
      swap 6 7
      swap 8 9
    16 -> do
      swap 0 1
      swap 2 3
      swap 4 5
      swap 6 7
      swap 8 9
      swap 10 11
      swap 12 13
      swap 14 15
      swap 0 2
      swap 4 6
      swap 8 10
      swap 12 14
      swap 1 3
      swap 5 7
      swap 9 11
      swap 13 15
      swap 0 4
      swap 8 12
      swap 1 5
      swap 9 13
      swap 2 6
      swap 10 14
      swap 3 7
      swap 11 15
      swap 0 8
      swap 1 9
      swap 2 10
      swap 3 11
      swap 4 12
      swap 5 13
      swap 6 14
      swap 7 15
      swap 5 10
      swap 6 9
      swap 3 12
      swap 13 14
      swap 7 11
      swap 1 2
      swap 4 8
      swap 1 4
      swap 7 13
      swap 2 8
      swap 11 14
      swap 5 6
      swap 9 10
      swap 2 4
      swap 11 13
      swap 3 8
      swap 7 12
      swap 6 8
      swap 10 12
      swap 3 5
      swap 7 9
      swap 3 4
      swap 5 6
      swap 7 8
      swap 9 10
      swap 11 12
      swap 6 7
      swap 8 9
    _ ->
      pure ()
  where
    swap :: Int -> Int -> m ()
    swap !i !j = do
      x <- GM.unsafeRead v i
      y <- GM.unsafeRead v j
      when (x > y) $ do
        GM.unsafeWrite v i y
        GM.unsafeWrite v j x
