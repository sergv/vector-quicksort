----------------------------------------------------------------------------
-- |
-- Module      :  BenchmarkMain
-- Copyright   :  (c) Sergey Vinokurov 2022
-- License     :  Apache-2.0 (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings        #-}

{-# OPTIONS_GHC -O2 -ddump-simpl -dsuppress-uniques -dsuppress-idinfo -dsuppress-module-prefixes -dsuppress-type-applications -dsuppress-coercions -dppr-cols200 -dsuppress-type-signatures -ddump-to-file #-}

{-# OPTIONS_GHC -fspecialise-aggressively #-}

module BenchmarkMain (main) where

import Prelude hiding (pi, last)

import Control.Concurrent.STM
import Control.Monad.ST
import Data.ByteString.Char8 qualified as C8
import Data.Int
import Data.Ord
import Data.Vector.Algorithms.Heap qualified as Heap
import Data.Vector.Primitive qualified as P
import Data.Vector.Primitive.Mutable qualified as PM
import Data.Vector.Storable qualified as S
import Data.Vector.Storable.Mutable qualified as SM
import Foreign.C.Types
import Test.Tasty.Bench
import Test.Tasty.Patterns.Printer (printAwkExpr)

import ForeignSorting

import Data.Vector.Algorithms.Heapsort qualified as Heapsort

import Data.Vector.Algorithms.Quicksort.Predefined.PIntParallelStrategiesMedian3IO
import Data.Vector.Algorithms.Quicksort.Predefined.PIntParallelStrategiesMedian3ST
import Data.Vector.Algorithms.Quicksort.Predefined.PIntParallelStrategiesMedian3or5IO
import Data.Vector.Algorithms.Quicksort.Predefined.PIntParallelStrategiesMedian3or5ST
import Data.Vector.Algorithms.Quicksort.Predefined.PIntSequentialMedian3IO
import Data.Vector.Algorithms.Quicksort.Predefined.PIntSequentialMedian3ST
import Data.Vector.Algorithms.Quicksort.Predefined.PIntSequentialMedian3or5ST

newtype SortKey v = SortKey { _unSortKey :: (Int32, Int, v) }

instance Eq (SortKey v) where
  SortKey (a, b, _) == SortKey (a', b', _) = a == a' && b == b'

instance Ord (SortKey v) where
  SortKey (a, b, _) `compare` SortKey (a', b', _) = Down a `compare` Down a' <> b `compare` b'

{-# NOINLINE qsortSeq3 #-}
qsortSeq3 :: P.Vector Int64 -> ST s (PM.MVector s Int64)
qsortSeq3 xs = do
  ys <- P.thaw xs
  -- Quick.sort Sequential (Median3 @Int64) ys
  sortPIntSequentialMedian3ST ys
  pure ys

{-# NOINLINE qsortSeqIO3 #-}
qsortSeqIO3 :: P.Vector Int64 -> IO (PM.MVector RealWorld Int64)
qsortSeqIO3 xs = do
  ys <- P.thaw xs
  -- stToIO $ Quick.sort Sequential (Median3 @Int64) ys
  sortPIntSequentialMedian3IO ys
  pure ys


-- {-# NOINLINE qsortPar3 #-}
-- qsortPar3 :: P.Vector Int64 -> IO (PM.MVector RealWorld Int64)
-- qsortPar3 xs = do
--   ys <- P.thaw xs
--   n  <- getNumCapabilities
--   p  <- mkParallel n
--   Quick.sort p (Median3 @Int64) ys
--   waitParallel p
--   pure ys

{-# NOINLINE qsortParStrategies3IO #-}
qsortParStrategies3IO :: P.Vector Int64 -> IO (PM.MVector RealWorld Int64)
qsortParStrategies3IO xs = do
  ys <- P.thaw xs
  -- Quick.sort ParStrategies (Median3 @Int64) ys
  sortPIntParallelStrategiesMedian3IO ys
  pure ys

{-# NOINLINE qsortParStrategies3ST #-}
qsortParStrategies3ST :: P.Vector Int64 -> ST s (PM.MVector s Int64)
qsortParStrategies3ST xs = do
  ys <- P.thaw xs
  -- Quick.sort ParStrategies (Median3 @Int64) ys
  sortPIntParallelStrategiesMedian3ST ys
  pure ys



{-# NOINLINE qsortSeq3or5 #-}
qsortSeq3or5 :: P.Vector Int64 -> IO (PM.MVector RealWorld Int64)
qsortSeq3or5 xs = do
  ys <- P.thaw xs
  -- stToIO $ Quick.sort Sequential (Median3or5 @Int64) ys
  stToIO $ sortPIntSequentialMedian3or5ST ys
  pure ys

-- {-# NOINLINE qsortPar3or5 #-}
-- qsortPar3or5 :: P.Vector Int64 -> IO (PM.MVector RealWorld Int64)
-- qsortPar3or5 xs = do
--   ys <- P.thaw xs
--   n  <- getNumCapabilities
--   p  <- mkParallel n
--   Quick.sort p (Median3or5 @Int64) ys
--   waitParallel p
--   pure ys

{-# NOINLINE qsortParStrategies3or5IO #-}
qsortParStrategies3or5IO :: P.Vector Int64 -> IO (PM.MVector RealWorld Int64)
qsortParStrategies3or5IO xs = do
  ys <- P.thaw xs
  sortPIntParallelStrategiesMedian3or5IO ys
  -- Quick.sort ParStrategies (Median3or5 @Int64) ys
  pure ys

{-# NOINLINE qsortParStrategies3or5ST #-}
qsortParStrategies3or5ST :: P.Vector Int64 -> ST s (PM.MVector s Int64)
qsortParStrategies3or5ST xs = do
  ys <- P.thaw xs
  sortPIntParallelStrategiesMedian3or5ST ys
  -- Quick.sort ParStrategies (Median3or5 @Int64) ys
  pure ys

{-# NOINLINE vectorAlgoHeapsortInt64 #-}
vectorAlgoHeapsortInt64 :: P.Vector Int64 -> ST s (PM.MVector s Int64)
vectorAlgoHeapsortInt64 xs = do
  ys <- P.thaw xs
  Heap.sort ys
  pure ys

{-# NOINLINE fallbackHeapsortInt64 #-}
fallbackHeapsortInt64 :: P.Vector Int64 -> ST s (PM.MVector s Int64)
fallbackHeapsortInt64 xs = do
  ys <- P.thaw xs
  Heapsort.heapSort ys
  pure ys

{-# NOINLINE cppUnboxedInt64 #-}
cppUnboxedInt64 :: S.Vector Int64 -> IO (SM.MVector RealWorld Int64)
cppUnboxedInt64 xs = do
  ys <- S.thaw xs
  SM.unsafeWith ys $ \ptr -> cppSortInt64 ptr (CInt (fromIntegral (SM.length ys)))
  pure ys



incrementTVar :: TVar Int -> Int -> IO ()
incrementTVar v = go
  where
    go 0 = pure ()
    go n = do
      atomically $ modifyTVar' v (+ 1)
      go (n - 1)


main :: IO ()
main = do
  (xsPrim :: P.Vector Int64) <-
    read . C8.unpack <$> C8.readFile "/home/sergey/projects/haskell/projects/vector-quicksort/test.txt"

  let xsStorable :: S.Vector Int64
      xsStorable = P.convert xsPrim

  putStrLn $ "P.length xsPrim = " ++ show (P.length xsPrim)

  v <- newTVarIO 0

  defaultMain
    [ bgroup "Overhead"
      [ bench "Increment TVar 50 times"     $ nfAppIO (incrementTVar v) 50
      , bench "Increment TVar 2000 times"   $ nfAppIO (incrementTVar v) 2000
      ]
    , bgroup "Sorting" $ map (mapLeafBenchmarks addCompare)
      [ bench cppBenchName                  $ nfAppIO cppUnboxedInt64 xsStorable
      , bench "vector-algorithms heapsort"  $ nfAppIO (stToIO . vectorAlgoHeapsortInt64) xsPrim
      , bench "fallback heapsort"           $ nfAppIO (stToIO . fallbackHeapsortInt64) xsPrim

      , bench "Sequential ST Median3"       $ nfAppIO (stToIO . qsortSeq3) xsPrim
      , bench "Sequential IO Median3"       $ nfAppIO qsortSeqIO3 xsPrim
      -- , bench "Parallel Median3"            $ nfAppIO qsortPar3 xsPrim
      , bench "ParStrategies IO Median3"    $ nfAppIO qsortParStrategies3IO xsPrim
      , bench "ParStrategies ST Median3"    $ nfAppIO (stToIO . qsortParStrategies3ST) xsPrim

      , bench "Sequential Median3or5"       $ nfAppIO qsortSeq3or5 xsPrim
      -- , bench "Parallel Median3or5"         $ nfAppIO qsortPar3or5 xsPrim
      , bench "ParStrategies IO Median3or5" $ nfAppIO qsortParStrategies3or5IO xsPrim
      , bench "ParStrategies ST Median3or5" $ nfAppIO (stToIO . qsortParStrategies3or5ST) xsPrim
      ]
    ]

cppBenchName :: String
cppBenchName = "C++"

addCompare :: [String] -> Benchmark -> Benchmark
addCompare (name : path)
  | name /= cppBenchName
  = bcompare (printAwkExpr (locateBenchmark (cppBenchName : path)))
addCompare _ = id
