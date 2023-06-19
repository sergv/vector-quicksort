-- |
-- Module:     ForeignSorting
-- Copyright:  (c) Sergey Vinokurov 2023
-- License:    Apache-2.0 (see LICENSE)
-- Maintainer: serg.foo@gmail.com

{-# LANGUAGE ForeignFunctionInterface #-}

module ForeignSorting
  ( cppSortInt64
  , cppSortCPoint
  , cppSortCPointUnboxed
  , CPoint(..)
  ) where

import Data.Int
import Data.Word
import Foreign
import Foreign.C.Types

#include <sort.h>

foreign import ccall unsafe "cplusplus_sort_int64" cppSortInt64
  :: Ptr Int64
  -> CInt
  -> IO ()

data CPoint = CPoint !CDouble !CDouble !Word64
  deriving (Eq, Show)



instance Storable CPoint where
   sizeOf    _ = (#size struct Point)
   alignment _ = (#alignment struct Point)
   peek p =
     CPoint
       <$> (#peek struct Point, x) p
       <*> (#peek struct Point, y) p
       <*> (#peek struct Point, id) p
   poke p (CPoint x y n) = do
     (#poke struct Point, x) p x
     (#poke struct Point, y) p y
     (#poke struct Point, id) p n

foreign import ccall unsafe "cplusplus_sort_point" cppSortCPoint
  :: Ptr CPoint
  -> CInt
  -> IO ()

foreign import ccall unsafe "cplusplus_sort_point_unboxed" cppSortCPointUnboxed
  :: Ptr CDouble
  -> Ptr CDouble
  -> Ptr Word64
  -> CInt
  -> IO ()
