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

module BenchMain (main) where

import Prelude hiding (pi, last)

import Control.Concurrent.STM
import Control.DeepSeq
import Control.Exception
import Control.Monad
import Control.Monad.ST
import Data.ByteString.Char8 qualified as C8
import Data.Coerce
import Data.Foldable
import Data.Int
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Builder.Linear qualified as TBL
import Data.Tuple
import Data.Vector.Algorithms.Heap qualified as Heap
import Data.Vector.Primitive qualified as P
import Data.Vector.Primitive.Mutable qualified as PM
import Data.Vector.Storable qualified as S
import Data.Vector.Storable.Mutable qualified as SM
import Data.Vector.Unboxed qualified as U
import Data.Vector.Unboxed.Mutable qualified as UM
import Data.Word
import Foreign.C.Types
import System.Random.Stateful

import Test.Tasty (localOption)
import Test.Tasty.Bench
import Test.Tasty.HUnit
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


import Data.Vector.Algorithms.Quicksort.Predefined.SortTriple
import Data.Vector.Algorithms.Quicksort.Predefined.PTripleSequentialMedian3ST
import Data.Vector.Algorithms.Quicksort.Predefined.PTripleSequentialMedian3or5ST
import Data.Vector.Algorithms.Quicksort.Predefined.UTripleSequentialMedian3ST
import Data.Vector.Algorithms.Quicksort.Predefined.UTripleSequentialMedian3or5ST

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

{-# NOINLINE cppStorableTriple #-}
cppStorableTriple :: S.Vector CPoint -> IO (SM.MVector RealWorld CPoint)
cppStorableTriple xs = do
  ys <- S.thaw xs
  SM.unsafeWith ys $ \ptr -> cppSortCPoint ptr (CInt (fromIntegral (SM.length ys)))
  pure ys

{-# NOINLINE cppStorableTripleUnpacked #-}
cppStorableTripleUnpacked
  :: (S.Vector CDouble, S.Vector CDouble, S.Vector Word64)
  -> IO (SM.MVector RealWorld CDouble, SM.MVector RealWorld CDouble, SM.MVector RealWorld Word64)
cppStorableTripleUnpacked (as, bs, cs) = do
  as' <- S.thaw as
  bs' <- S.thaw bs
  cs' <- S.thaw cs
  SM.unsafeWith as' $ \as'' ->
    SM.unsafeWith bs' $ \bs'' ->
      SM.unsafeWith cs' $ \cs'' -> cppSortCPointUnboxed as'' bs'' cs'' (CInt (fromIntegral (SM.length as')))
  pure (as', bs', cs')

{-# NOINLINE qsortSeq3STTripleUnbox #-}
qsortSeq3STTripleUnbox
  :: forall s.
     U.Vector (Double, Double, Word64)
  -> ST s (UM.MVector s (Double, Double, Word64))
qsortSeq3STTripleUnbox xs = do
  (ys :: UM.MVector s (Double, Double, Word64)) <- U.thaw xs
  sortUTripleSequentialMedian3ST (coerce ys :: UM.MVector s SortTriple)
  pure ys

{-# NOINLINE qsortSeq3or5STTripleUnbox #-}
qsortSeq3or5STTripleUnbox
  :: forall s.
     U.Vector (Double, Double, Word64)
  -> ST s (UM.MVector s (Double, Double, Word64))
qsortSeq3or5STTripleUnbox xs = do
  (ys :: UM.MVector s (Double, Double, Word64)) <- U.thaw xs
  sortUTripleSequentialMedian3or5ST (coerce ys :: UM.MVector s SortTriple)
  pure ys


{-# NOINLINE qsortSeq3STTriplePrim #-}
qsortSeq3STTriplePrim
  :: forall s.
     P.Vector SortTriple
  -> ST s (PM.MVector s SortTriple)
qsortSeq3STTriplePrim xs = do
  (ys :: PM.MVector s SortTriple) <- P.thaw xs
  sortPTripleSequentialMedian3ST ys
  pure ys

{-# NOINLINE qsortSeq3or5STTriplePrim #-}
qsortSeq3or5STTriplePrim
  :: forall s.
     P.Vector SortTriple
  -> ST s (PM.MVector s SortTriple)
qsortSeq3or5STTriplePrim xs = do
  (ys :: PM.MVector s SortTriple) <- P.thaw xs
  sortPTripleSequentialMedian3or5ST ys
  pure ys


main :: IO ()
main = do
  (fuzzyMatchScores :: P.Vector Int64) <-
    read . C8.unpack <$> C8.readFile "test.txt"

  putStrLn $ "P.length fuzzyMatchScores = " ++ show (P.length fuzzyMatchScores)

  let generateInt64 :: Int -> Int -> IOGenM StdGen -> IO (P.Vector Int64)
      generateInt64 n k g = P.replicateM n (uniformRM (1 :: Int64, fromIntegral k) g)

  let generateTriple :: Int -> Int -> IOGenM StdGen -> IO (U.Vector (Double, Double, Word64))
      generateTriple n k g =
        U.replicateM n $
          (,,)
            <$> uniformDouble01M g
            <*> uniformDouble01M g
            <*> uniformRM (1 :: Word64, fromIntegral k) g

  gen       <- newIOGenM $ mkStdGen 1
  let sizes :: [(Int, Int)]
      sizes = map (10, ) [16, 17, 100, 256, 1000, 10_000, 100_000, 1_000_000] -- [, 10_000_000]

  (xsssNoDup :: [[P.Vector Int64]]) <- traverse (\(n, k) -> replicateM n $ generateInt64 k k gen) sizes
  (xsssDup   :: [[P.Vector Int64]]) <- traverse (\(n, k) -> replicateM n $ generateInt64 k (max 1000 (k `quot` 1000)) gen) sizes

  (ysssNoDup :: [[U.Vector (Double, Double, Word64)]]) <- traverse (\(n, k) -> replicateM n $ generateTriple k k gen) sizes
  (ysssDup   :: [[U.Vector (Double, Double, Word64)]]) <- traverse (\(n, k) -> replicateM n $ generateTriple k (max 1000 (k `quot` 1000)) gen) sizes

  evaluate $ rnf xsssNoDup
  evaluate $ rnf xsssDup
  evaluate $ rnf ysssNoDup
  evaluate $ rnf ysssDup

  defaultMain $ map (localOption WallTime) $
    [ mkBenchesInt64 "Sorting fuzzy matching scores vector" (MkSolo fuzzyMatchScores)
    ] ++
    [ bgroup "Int64" $
      [ mkBenchesInt64 ("Sorting " ++ show (length xss) ++ " random arrays of length " ++ T.unpack (formatNumber (P.length (head xss))) ++ " with few duplicates") xss
      | xss <- xsssNoDup
      ] ++
      [ mkBenchesInt64 ("Sorting " ++ show (length xss) ++ " random arrays of length " ++ T.unpack (formatNumber (P.length (head xss))) ++ " with many duplicates") xss
      | xss <- xsssDup
      ]
    , bgroup "(Double, Double, Int64)" $
      [ mkBenchesTriple ("Sorting " ++ show (length yss) ++ " random arrays of length " ++ T.unpack (formatNumber (U.length (head yss))) ++ " with few duplicates") yss
      | yss <- ysssNoDup
      ] ++
      [ mkBenchesTriple ("Sorting " ++ show (length yss) ++ " random arrays of length " ++ T.unpack (formatNumber (U.length (head yss))) ++ " with many duplicates") yss
      | yss <- ysssDup
      ]
    ]

instance NFData (TVar a) where
  rnf x = x `seq` ()

{-# INLINE mkBenchesInt64 #-}
mkBenchesInt64
  :: forall f. (Traversable f, forall a. NFData a => NFData (f a))
  => String -> f (P.Vector Int64) -> Benchmark
mkBenchesInt64 name xssPrim = mapLeafBenchmarks addCompare $ bgroup name
  [ bench cppBenchName                    $ nfAppIO (traverse cppUnboxedInt64) xssStorable

  , bench "Sequential ST Median3"         $ nfAppIO (stToIO . traverse qsortSeq3) xssPrim
  , bench "Sequential IO Median3"         $ nfAppIO (traverse qsortSeqIO3) xssPrim
  , bench "ParStrategies ST Median3"      $ nfAppIO (stToIO . traverse qsortParStrategies3ST) xssPrim
  , bench "ParStrategies IO Median3"      $ nfAppIO (traverse qsortParStrategies3IO) xssPrim

  , bench "Sequential ST Median3or5"      $ nfAppIO (stToIO . traverse qsortSeq3or5ST) xssPrim
  , bench "Sequential IO Median3or5"      $ nfAppIO (traverse qsortSeq3or5IO) xssPrim
  , bench "ParStrategies ST Median3or5"   $ nfAppIO (stToIO . traverse qsortParStrategies3or5ST) xssPrim
  , bench "ParStrategies IO Median3or5"   $ nfAppIO (traverse qsortParStrategies3or5IO) xssPrim

  , bench "Threads IO Median3"            $ nfAppIO (traverse qsortParallel3IO) xssPrim
  , bench "Threads IO Median3or5"         $ nfAppIO (traverse qsortParallel3or5IO) xssPrim

  , bench "vector-algorithms heapsort"    $ nfAppIO (stToIO . traverse vectorAlgoHeapsortInt64) xssPrim
  , bench "fallback heapsort"             $ nfAppIO (stToIO . traverse fallbackHeapsortInt64) xssPrim
  ]
  where
    xssStorable :: f (S.Vector Int64)
    xssStorable = P.convert <$> xssPrim

isSorted :: Ord a => [a] -> Bool
isSorted = \case
  []     -> True
  x : xs -> go x xs
  where
    go _ []       = True
    go x (y : ys) = x <= y && go y ys

assertSorted :: (Show a, Ord a) => String -> [a] -> IO ()
assertSorted label xs =
  assertBool (label ++ " not sorted: " ++ show xs) $ isSorted xs

{-# INLINE mkBenchesTriple #-}
mkBenchesTriple
  :: forall f. (Traversable f, forall a. NFData a => NFData (f a))
  => String -> f (U.Vector (Double, Double, Word64)) -> Benchmark
mkBenchesTriple name xssUnbox = bgroup name
  [ testCase "Sanity" $
      for_ xssUnbox $ \xs -> do
        xsUnboxed  <- U.unsafeFreeze =<< stToIO (qsortSeq3STTripleUnbox xs)
        xsStorable <- S.unsafeFreeze =<< cppStorableTriple (mkStorable xs)
        xsUnpacked <-
          (\(a, b, c) -> (,,) <$> S.unsafeFreeze a <*> S.unsafeFreeze b <*> S.unsafeFreeze c) =<<
          cppStorableTripleUnpacked (mkUnpacked xs)
        xsPrim     <- P.unsafeFreeze =<< stToIO (qsortSeq3STTriplePrim (mkPrim xs))
        assertSorted "Unboxed"   $ map (\(_, _, c) -> c) $ U.toList xsUnboxed
        assertSorted "Storabale" $ map (\(CPoint _ _ z) -> z) $ S.toList xsStorable
        assertSorted "Unpacked"  $ S.toList $ (\(_, _, c) -> c) xsUnpacked
        assertSorted "Prim"      $ map (\(SortTriple _ _ c) -> c) $ P.toList $ xsPrim

  , bench "C++ single vector"              $ nfAppIO (traverse cppStorableTriple) xssStorable
  , bench "C++ three vectors"              $ nfAppIO (traverse cppStorableTripleUnpacked) xssUnpacked

  , bench "Sequential ST Median3 Unbox"    $ nfAppIO (stToIO . traverse qsortSeq3STTripleUnbox) xssUnbox
  , bench "Sequential ST Median3or5 Unbox" $ nfAppIO (stToIO . traverse qsortSeq3or5STTripleUnbox) xssUnbox

  , bench "Sequential ST Median3 Prim"     $ nfAppIO (stToIO . traverse qsortSeq3STTriplePrim) xssPrim
  , bench "Sequential ST Median3or5 Prim"  $ nfAppIO (stToIO . traverse qsortSeq3or5STTriplePrim) xssPrim
  ]
  where
    mkStorable
      :: U.Vector (Double, Double, Word64)
      -> S.Vector CPoint
    mkStorable = S.fromList . map (\(a, b, c) -> CPoint (CDouble a) (CDouble b) c) . U.toList

    mkPrim
      :: U.Vector (Double, Double, Word64)
      -> P.Vector SortTriple
    mkPrim = U.convert . U.map (\(a, b, c) -> SortTriple a b c)

    mkUnpacked
      :: U.Vector (Double, Double, Word64)
      -> (S.Vector CDouble, S.Vector CDouble, S.Vector Word64)
    mkUnpacked xs =
      ( S.fromList . map (\(a, _, _) -> CDouble a) . U.toList $ xs
      , S.fromList . map (\(_, b, _) -> CDouble b) . U.toList $ xs
      , U.convert . U.map (\(_, _, c) -> c) $ xs
      )

    xssStorable :: f (S.Vector CPoint)
    xssStorable = mkStorable <$> xssUnbox

    xssUnpacked :: f (S.Vector CDouble, S.Vector CDouble, S.Vector Word64)
    xssUnpacked = mkUnpacked <$> xssUnbox

    xssPrim :: f (P.Vector SortTriple)
    xssPrim = mkPrim <$> xssUnbox

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

formatNumber :: Int -> Text
formatNumber x = TBL.runBuilder $ sign <> go mempty (abs x)
  where
    sign :: TBL.Builder
    sign = if x < 0 then "-" else ""
    go :: TBL.Builder -> Int -> TBL.Builder
    go acc n
      | n < 1000  = TBL.fromDec n <> acc
      | otherwise = go (TBL.fromChar ',' <> padding <> TBL.fromText k' <> acc) n'
      where
        (n', k) = n `quotRem` 1000
        k'      = TBL.runBuilder $ TBL.fromDec k
        padding = TBL.fromText $ T.replicate (3 - T.length k') $ T.singleton '0'
