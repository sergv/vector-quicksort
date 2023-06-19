-- |
-- Module:     Data.Vector.Algorithms.Quicksort.Predefined.SortTriple
-- Copyright:  (c) Sergey Vinokurov 2023
-- License:    Apache-2.0 (see LICENSE)
-- Maintainer: serg.foo@gmail.com

{-# LANGUAGE DerivingVia   #-}
{-# LANGUAGE InstanceSigs  #-}
{-# LANGUAGE MagicHash     #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE UnboxedTuples #-}

module Data.Vector.Algorithms.Quicksort.Predefined.SortTriple
  ( SortTriple(..)
  , U.MVector(..)
  , U.Vector(..)
  ) where

import Data.Word
import Data.Vector.Generic qualified as G
import Data.Vector.Generic.Mutable qualified as GM
import Data.Vector.Unboxed qualified as U

import Data.Primitive.Types
import GHC.Exts
import GHC.Word

data SortTriple = SortTriple !Double !Double !Word64

instance Eq SortTriple where
  SortTriple _ _ c == SortTriple _ _ c' = c == c'

instance Ord SortTriple where
  SortTriple _ _ c `compare` SortTriple _ _ c' = compare c c'

instance U.IsoUnbox SortTriple (Double, Double, Word64) where
  toURepr (SortTriple a b c) = (a, b, c)
  fromURepr (a, b, c) = SortTriple a b c
  {-# INLINE toURepr   #-}
  {-# INLINE fromURepr #-}

newtype instance U.MVector s SortTriple = MV_TestPair (U.MVector s (Double, Double, Word64))
newtype instance U.Vector    SortTriple = V_TestPair  (U.Vector    (Double, Double, Word64))
deriving via (SortTriple `U.As` (Double, Double, Word64)) instance GM.MVector U.MVector SortTriple
deriving via (SortTriple `U.As` (Double, Double, Word64)) instance G.Vector   U.Vector  SortTriple
instance U.Unbox SortTriple

idxs :: Int# -> (# Int#, Int#, Int# #)
idxs idx =
  (# start
  , start +# sizeOf# (undefined :: Double)
  , start +# sizeOf# (undefined :: Double) +# sizeOf# (undefined :: Double)
  #)
  where
    start = idx *# sizeOf# (undefined :: SortTriple)

instance Prim SortTriple where
  sizeOf# _
    =  sizeOf# (undefined :: Double)
    +# sizeOf# (undefined :: Double)
    +# sizeOf# (undefined :: Word64)

  alignment# _
    = alignment# (undefined :: Word64)

  indexByteArray# :: ByteArray# -> Int# -> SortTriple
  indexByteArray# arr idx =
    SortTriple
      (D#   (indexWord8ArrayAsDouble# arr ia))
      (D#   (indexWord8ArrayAsDouble# arr ib))
      (W64# (indexWord8ArrayAsWord64# arr ic))
    where
      !(# ia, ib, ic #) = idxs idx

  readByteArray# :: MutableByteArray# s -> Int# -> State# s -> (# State# s, SortTriple #)
  readByteArray# arr idx s1 =
    case readWord8ArrayAsDouble# arr ia s1 of
      (# s2, a #) ->
        case readWord8ArrayAsDouble# arr ib s2 of
          (# s3, b #) ->
            case readWord8ArrayAsWord64# arr ic s3 of
              (# s4, c #) -> (# s4, SortTriple (D# a) (D# b) (W64# c) #)
    where
      !(# ia, ib, ic #) = idxs idx

  writeByteArray# :: MutableByteArray# s -> Int# -> SortTriple -> State# s -> State# s
  writeByteArray# arr idx (SortTriple (D# a) (D# b) (W64# c)) s1 =
    case writeWord8ArrayAsDouble# arr ia a s1 of
      s2 ->
        case writeWord8ArrayAsDouble# arr ib b s2 of
          s3 ->
            writeWord8ArrayAsWord64# arr ic c s3
    where
      !(# ia, ib, ic #) = idxs idx

  setByteArray#
    :: MutableByteArray# s
    -> Int# -- ^ offset
    -> Int# -- ^ length
    -> SortTriple
    -> State# s
    -> State# s
  setByteArray# = defaultSetByteArray#

  indexOffAddr# :: Addr# -> Int# -> SortTriple
  indexOffAddr# addr idx =
    SortTriple
      (D#   (indexDoubleOffAddr# addr ia))
      (D#   (indexDoubleOffAddr# addr ib))
      (W64# (indexWord64OffAddr# addr ic))
    where
      !(# ia, ib, ic #) = idxs idx

  readOffAddr# :: Addr# -> Int# -> State# s -> (# State# s, SortTriple #)
  readOffAddr# addr idx s1 =
    case readDoubleOffAddr# addr ia s1 of
      (# s2, a #) ->
        case readDoubleOffAddr# addr ib s2 of
          (# s3, b #) ->
            case readWord64OffAddr# addr ic s3 of
              (# s4, c #) -> (# s4, SortTriple (D# a) (D# b) (W64# c) #)
    where
      !(# ia, ib, ic #) = idxs idx

  writeOffAddr# :: Addr# -> Int# -> SortTriple -> State# s -> State# s
  writeOffAddr# addr idx (SortTriple (D# a) (D# b) (W64# c)) s1 =
    case writeDoubleOffAddr# addr ia a s1 of
      s2 ->
        case writeDoubleOffAddr# addr ib b s2 of
          s3 ->
            writeWord64OffAddr# addr ic c s3
    where
      !(# ia, ib, ic #) = idxs idx

  setOffAddr#
    :: Addr#
    -> Int# -- ^ offset
    -> Int# -- ^ length
    -> SortTriple
    -> State# s
    -> State# s
  setOffAddr# = defaultSetOffAddr#

