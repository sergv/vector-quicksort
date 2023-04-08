-- |
-- Module:     BenchmarkMain
-- Copyright:  (c) Sergey Vinokurov 2022
-- License:    Apache-2.0 (see LICENSE)
-- Maintainer: serg.foo@gmail.com

{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE PatternSynonyms          #-}
{-# LANGUAGE QuantifiedConstraints    #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module BenchmarkMain (main) where

import Prelude hiding (pi, last)

import Control.Concurrent.STM
import Control.DeepSeq
import Control.Monad
import Control.Monad.ST
import Data.ByteString.Char8 qualified as C8
import Data.Int
import Data.Ord
import Data.Tuple
import Data.Vector.Algorithms.Heap qualified as Heap
import Data.Vector.Primitive qualified as P
import Data.Vector.Primitive.Mutable qualified as PM
import Data.Vector.Storable qualified as S
import Data.Vector.Storable.Mutable qualified as SM
import Foreign.C.Types
import System.Random.Stateful

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
import Data.Vector.Algorithms.Quicksort.Predefined.PIntSequentialMedian3or5IO
import Data.Vector.Algorithms.Quicksort.Predefined.PIntSequentialMedian3or5ST

import Data.Vector.Algorithms.Quicksort.Predefined.PIntParallelMedian3IO
import Data.Vector.Algorithms.Quicksort.Predefined.PIntParallelMedian3or5IO

newtype SortKey v = SortKey { _unSortKey :: (Int32, Int, v) }

instance Eq (SortKey v) where
  SortKey (a, b, _) == SortKey (a', b', _) = a == a' && b == b'

instance Ord (SortKey v) where
  SortKey (a, b, _) `compare` SortKey (a', b', _) = Down a `compare` Down a' <> b `compare` b'

{-# NOINLINE qsortSeq3 #-}
qsortSeq3 :: P.Vector Int64 -> ST s (PM.MVector s Int64)
qsortSeq3 xs = do
  ys <- P.thaw xs
  sortPIntSequentialMedian3ST ys
  pure ys

{-# NOINLINE qsortSeqIO3 #-}
qsortSeqIO3 :: P.Vector Int64 -> IO (PM.MVector RealWorld Int64)
qsortSeqIO3 xs = do
  ys <- P.thaw xs
  sortPIntSequentialMedian3IO ys
  pure ys

{-# NOINLINE qsortParStrategies3IO #-}
qsortParStrategies3IO :: P.Vector Int64 -> IO (PM.MVector RealWorld Int64)
qsortParStrategies3IO xs = do
  ys <- P.thaw xs
  sortPIntParallelStrategiesMedian3IO ys
  pure ys

{-# NOINLINE qsortParStrategies3ST #-}
qsortParStrategies3ST :: P.Vector Int64 -> ST s (PM.MVector s Int64)
qsortParStrategies3ST xs = do
  ys <- P.thaw xs
  sortPIntParallelStrategiesMedian3ST ys
  pure ys

{-# NOINLINE qsortSeq3or5ST #-}
qsortSeq3or5ST :: P.Vector Int64 -> ST s (PM.MVector s Int64)
qsortSeq3or5ST xs = do
  ys <- P.thaw xs
  sortPIntSequentialMedian3or5ST ys
  pure ys

{-# NOINLINE qsortSeq3or5IO #-}
qsortSeq3or5IO :: P.Vector Int64 -> IO (PM.MVector RealWorld Int64)
qsortSeq3or5IO xs = do
  ys <- P.thaw xs
  sortPIntSequentialMedian3or5IO ys
  pure ys

{-# NOINLINE qsortParStrategies3or5IO #-}
qsortParStrategies3or5IO :: P.Vector Int64 -> IO (PM.MVector RealWorld Int64)
qsortParStrategies3or5IO xs = do
  ys <- P.thaw xs
  sortPIntParallelStrategiesMedian3or5IO ys
  pure ys

{-# NOINLINE qsortParStrategies3or5ST #-}
qsortParStrategies3or5ST :: P.Vector Int64 -> ST s (PM.MVector s Int64)
qsortParStrategies3or5ST xs = do
  ys <- P.thaw xs
  sortPIntParallelStrategiesMedian3or5ST ys
  pure ys

{-# NOINLINE qsortParallel3IO #-}
qsortParallel3IO :: P.Vector Int64 -> IO (PM.MVector RealWorld Int64)
qsortParallel3IO xs = do
  ys <- P.thaw xs
  sortPIntParallelMedian3IO ys
  pure ys

{-# NOINLINE qsortParallel3or5IO #-}
qsortParallel3or5IO :: P.Vector Int64 -> IO (PM.MVector RealWorld Int64)
qsortParallel3or5IO xs = do
  ys <- P.thaw xs
  sortPIntParallelMedian3or5IO ys
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

main :: IO ()
main = do
  (fuzzyMatchScores :: P.Vector Int64) <-
    read . C8.unpack <$> C8.readFile "test.txt"

  putStrLn $ "P.length fuzzyMatchScores = " ++ show (P.length fuzzyMatchScores)

  let generate n g = P.fromList <$> replicateM n (uniformRM (1 :: Int64, 128) g)

  gen       <- newIOGenM $ mkStdGen 1
  let n :: Int
      n = 1000
  xssSmall16  <- replicateM n $ generate 16 gen
  xssSmall17  <- replicateM n $ generate 17 gen
  xssMid100   <- replicateM n $ generate 100 gen
  xssMid      <- replicateM n $ generate 256 gen
  xssLarge    <- replicateM n $ generate 20000 gen
  xssHuge     <- replicateM n $ generate (P.length fuzzyMatchScores) gen

  defaultMain $
    [ bgroup "Overhead thaw" $
      [ bench "fuzzy scores vector" $ nfAppIO P.thaw fuzzyMatchScores
      ] ++
      [ bench (show (length xss) ++ " vectors of length " ++ show (P.length (head xss))) $
        nfAppIO (traverse P.thaw) xss
      | xss <-
        [ xssSmall16
        , xssSmall17
        , xssMid100
        , xssMid
        , xssLarge
        , xssHuge
        ]
      ]
    ] ++
    [ mkBenches "Sorting fuzzy matching scores vector" (MkSolo fuzzyMatchScores)
    ] ++
    [ mkBenches ("Sorting " ++ show (length xss) ++ " random arrays of length " ++ show (P.length (head xss))) xss
    | xss <-
      [ xssSmall16
      , xssSmall17
      , xssMid100
      , xssMid
      , xssLarge
      , xssHuge
      ]
    ]

instance NFData (TVar a) where
  rnf x = x `seq` ()

{-# INLINE mkBenches #-}
mkBenches
  :: forall f. (Traversable f, forall a. NFData a => NFData (f a))
  => String -> f (P.Vector Int64) -> Benchmark
mkBenches name xssPrim = mapLeafBenchmarks addCompare $ bgroup name
  [ bench cppBenchName                  $ nfAppIO (traverse cppUnboxedInt64) xsStorable

  , bench "Sequential ST Median3"       $ nfAppIO (stToIO . traverse qsortSeq3) xssPrim
  , bench "Sequential IO Median3"       $ nfAppIO (traverse qsortSeqIO3) xssPrim
  , bench "ParStrategies ST Median3"    $ nfAppIO (stToIO . traverse qsortParStrategies3ST) xssPrim
  , bench "ParStrategies IO Median3"    $ nfAppIO (traverse qsortParStrategies3IO) xssPrim

  , bench "Sequential ST Median3or5"    $ nfAppIO (stToIO . traverse qsortSeq3or5ST) xssPrim
  , bench "Sequential IO Median3or5"    $ nfAppIO (traverse qsortSeq3or5IO) xssPrim
  , bench "ParStrategies ST Median3or5" $ nfAppIO (stToIO . traverse qsortParStrategies3or5ST) xssPrim
  , bench "ParStrategies IO Median3or5" $ nfAppIO (traverse qsortParStrategies3or5IO) xssPrim

  , bench "Threads IO Median3"          $ nfAppIO (traverse qsortParallel3IO) xssPrim
  , bench "Threads IO Median3or5"       $ nfAppIO (traverse qsortParallel3or5IO) xssPrim

  , bench "vector-algorithms heapsort"  $ nfAppIO (stToIO . traverse vectorAlgoHeapsortInt64) xssPrim
  , bench "fallback heapsort"           $ nfAppIO (stToIO . traverse fallbackHeapsortInt64) xssPrim
  ]
  where
    xsStorable :: f (S.Vector Int64)
    xsStorable = P.convert <$> xssPrim

cppBenchName :: String
cppBenchName = "C++"

addCompare :: [String] -> Benchmark -> Benchmark
addCompare (name : path)
  | name /= cppBenchName
  = bcompare (printAwkExpr (locateBenchmark (cppBenchName : path)))
addCompare _ = id

#if !MIN_VERSION_base(4, 18, 0)
pattern MkSolo :: a -> Solo a
pattern MkSolo x = Solo x

# if !MIN_VERSION_base(4, 17, 0)
instance NFData a => NFData (Solo a)
# endif
#endif
