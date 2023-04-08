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

{-# OPTIONS_GHC -O2 #-}

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

import Test.Tasty.PAPI qualified as PAPI
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
  (fuzzyMatchScores :: P.Vector Int64) <-
    read . C8.unpack <$> C8.readFile "/home/sergey/projects/haskell/projects/vector-quicksort/test.txt"

  putStrLn $ "P.length fuzzyMatchScores = " ++ show (P.length fuzzyMatchScores)

  let generate n g = P.fromList <$> replicateM n (uniformRM (1 :: Int64, 128) g)

  gen       <- newIOGenM $ mkStdGen 1
  -- xsSmall15 <- generate 15 gen
  let n :: Int
      n = 1000
  xssSmall16 <- replicateM n $ generate 16 gen
  xssSmall17 <- replicateM n $ generate 17 gen
  xssMid100  <- replicateM n $ generate 100 gen
  xssMid     <- replicateM n $ generate 256 gen
  xssLarge   <- replicateM n $ generate 20000 gen

  v <- newTVarIO 0

  defaultMain $
    [ bgroup "Overhead TVar"
      [ bench "Increment TVar 50 times"     $ nfAppIO (incrementTVar v) 50
      , bench "Increment TVar 2000 times"   $ nfAppIO (incrementTVar v) 2000
      ]
    , bgroup "Overhead thaw" $
      [ bench "fuzzy scores vector" $ nfAppIO P.thaw fuzzyMatchScores
      ] ++
      [ bench (show (length xss) ++ " vectors of length " ++ show (P.length (head xss))) $ nfAppIO (traverse P.thaw) xss
      | xss <-
        [ xssSmall16
        , xssSmall17
        , xssMid100
        , xssMid
        , xssLarge
        ]
      ]
    , mkBenches "Sorting fuzzy matching scores vector" (MkSolo fuzzyMatchScores)
    ] ++
    [ mkBenches ("Sorting " ++ show (length xss) ++ " random arrays of length " ++ show (P.length (head xss))) xss
    | xss <-
      [ xssSmall16
      , xssSmall17
      , xssMid100
      , xssMid
      , xssLarge
      ]
    ]

{-# INLINE mkBenches #-}
mkBenches
  :: forall f. (Traversable f, forall a. NFData a => NFData (f a))
  => String -> f (P.Vector Int64) -> Benchmark
mkBenches name xssPrim = mapLeafBenchmarks addCompare $ bgroup name
  [ bench cppBenchName                  $ nfAppIO (traverse cppUnboxedInt64) xsStorable
  , bench "vector-algorithms heapsort"  $ nfAppIO (traverse (stToIO . vectorAlgoHeapsortInt64)) xssPrim
  , bench "fallback heapsort"           $ nfAppIO (traverse (stToIO . fallbackHeapsortInt64)) xssPrim

  , bench "Sequential ST Median3"       $ nfAppIO (traverse (stToIO . qsortSeq3)) xssPrim
  , bench "Sequential IO Median3"       $ nfAppIO (traverse qsortSeqIO3) xssPrim
  -- , bench "Parallel Median3"            $ nfAppIO qsortPar3 xssPrim
  , bench "ParStrategies IO Median3"    $ nfAppIO (traverse qsortParStrategies3IO) xssPrim
  , bench "ParStrategies ST Median3"    $ nfAppIO (traverse (stToIO . qsortParStrategies3ST)) xssPrim

  , bench "Sequential Median3or5"       $ nfAppIO (traverse qsortSeq3or5) xssPrim
  -- , bench "Parallel Median3or5"         $ nfAppIO (traverse qsortPar3or5) xssPrim
  , bench "ParStrategies IO Median3or5" $ nfAppIO (traverse qsortParStrategies3or5IO) xssPrim
  , bench "ParStrategies ST Median3or5" $ nfAppIO (traverse (stToIO . qsortParStrategies3or5ST)) xssPrim
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
#endif
