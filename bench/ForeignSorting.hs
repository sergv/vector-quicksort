-- |
-- Module:     ForeignSorting
-- Copyright:  (c) Sergey Vinokurov 2023
-- License:    Apache-2.0 (see LICENSE)
-- Maintainer: serg.foo@gmail.com

module ForeignSorting
  ( cppSortInt64
  ) where

import Data.Int
import Foreign
import Foreign.C.Types

foreign import ccall unsafe "cplusplus_sort" cppSortInt64
  :: Ptr Int64
  -> CInt
  -> IO ()
