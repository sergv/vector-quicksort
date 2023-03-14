----------------------------------------------------------------------------
-- |
-- Module      :  Data.Vector.Algorithms.Quicksort.Tests
-- Copyright   :  (c) Sergey Vinokurov 2023
-- License     :  Apache-2.0 (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}

{-# OPTIONS_GHC -fspecialise-aggressively #-}

module Data.Vector.Algorithms.Quicksort.Tests (tests) where

import Control.Monad.ST
import Data.Function
import Data.Int
import Data.List qualified as L
import Data.Set qualified as S
import Data.Vector qualified as V
import Data.Vector.Generic qualified as G
import Data.Vector.Primitive qualified as P
import Data.Vector.Unboxed qualified as U
import Test.Tasty
import Test.Tasty.QuickCheck qualified as QC

import Data.Vector.Algorithms.Quicksort.Predefined.Pair

import Data.Vector.Algorithms.Quicksort.Predefined.PIntParallelMedian3IO
import Data.Vector.Algorithms.Quicksort.Predefined.PIntParallelMedian3or5IO

import Data.Vector.Algorithms.Quicksort.Predefined.VIntSequentialMedian3ST
import Data.Vector.Algorithms.Quicksort.Predefined.VIntSequentialMedian3IO
import Data.Vector.Algorithms.Quicksort.Predefined.VPairSequentialMedian3ST
import Data.Vector.Algorithms.Quicksort.Predefined.VPairSequentialMedian3IO
-- import Data.Vector.Algorithms.Quicksort.Predefined.VIntParallelStrategiesMedian3ST
-- import Data.Vector.Algorithms.Quicksort.Predefined.VIntParallelStrategiesMedian3IO
import Data.Vector.Algorithms.Quicksort.Predefined.VPairParallelStrategiesMedian3ST
import Data.Vector.Algorithms.Quicksort.Predefined.VPairParallelStrategiesMedian3IO

import Data.Vector.Algorithms.Quicksort.Predefined.VIntSequentialMedian3or5ST
import Data.Vector.Algorithms.Quicksort.Predefined.VIntSequentialMedian3or5IO
import Data.Vector.Algorithms.Quicksort.Predefined.VPairSequentialMedian3or5ST
import Data.Vector.Algorithms.Quicksort.Predefined.VPairSequentialMedian3or5IO
import Data.Vector.Algorithms.Quicksort.Predefined.VIntParallelStrategiesMedian3or5ST
import Data.Vector.Algorithms.Quicksort.Predefined.VIntParallelStrategiesMedian3or5IO
import Data.Vector.Algorithms.Quicksort.Predefined.VPairParallelStrategiesMedian3or5ST
import Data.Vector.Algorithms.Quicksort.Predefined.VPairParallelStrategiesMedian3or5IO

import Data.Vector.Algorithms.Quicksort.Predefined.UIntSequentialMedian3ST
import Data.Vector.Algorithms.Quicksort.Predefined.UIntSequentialMedian3IO
import Data.Vector.Algorithms.Quicksort.Predefined.UPairSequentialMedian3ST
import Data.Vector.Algorithms.Quicksort.Predefined.UPairSequentialMedian3IO
-- import Data.Vector.Algorithms.Quicksort.Predefined.UIntParallelStrategiesMedian3ST
-- import Data.Vector.Algorithms.Quicksort.Predefined.UIntParallelStrategiesMedian3IO
import Data.Vector.Algorithms.Quicksort.Predefined.UPairParallelStrategiesMedian3ST
import Data.Vector.Algorithms.Quicksort.Predefined.UPairParallelStrategiesMedian3IO

import Data.Vector.Algorithms.Quicksort.Predefined.UIntSequentialMedian3or5ST
import Data.Vector.Algorithms.Quicksort.Predefined.UIntSequentialMedian3or5IO
import Data.Vector.Algorithms.Quicksort.Predefined.UPairSequentialMedian3or5ST
import Data.Vector.Algorithms.Quicksort.Predefined.UPairSequentialMedian3or5IO
import Data.Vector.Algorithms.Quicksort.Predefined.UIntParallelStrategiesMedian3or5ST
import Data.Vector.Algorithms.Quicksort.Predefined.UIntParallelStrategiesMedian3or5IO
import Data.Vector.Algorithms.Quicksort.Predefined.UPairParallelStrategiesMedian3or5ST
import Data.Vector.Algorithms.Quicksort.Predefined.UPairParallelStrategiesMedian3or5IO

import Data.Vector.Algorithms.Quicksort.Predefined.VTupleSequentialMedian3ST
import Data.Vector.Algorithms.Quicksort.Predefined.VTupleSequentialMedian3IO
-- import Data.Vector.Algorithms.Quicksort.Predefined.VTupleParallelStrategiesMedian3ST
-- import Data.Vector.Algorithms.Quicksort.Predefined.VTupleParallelStrategiesMedian3IO
import Data.Vector.Algorithms.Quicksort.Predefined.VTupleSequentialMedian3or5ST
import Data.Vector.Algorithms.Quicksort.Predefined.VTupleSequentialMedian3or5IO
import Data.Vector.Algorithms.Quicksort.Predefined.VTupleParallelStrategiesMedian3or5ST
import Data.Vector.Algorithms.Quicksort.Predefined.VTupleParallelStrategiesMedian3or5IO
import Data.Vector.Algorithms.Quicksort.Predefined.UTupleSequentialMedian3ST
import Data.Vector.Algorithms.Quicksort.Predefined.UTupleSequentialMedian3IO
-- import Data.Vector.Algorithms.Quicksort.Predefined.UTupleParallelStrategiesMedian3ST
-- import Data.Vector.Algorithms.Quicksort.Predefined.UTupleParallelStrategiesMedian3IO
import Data.Vector.Algorithms.Quicksort.Predefined.UTupleSequentialMedian3or5ST
import Data.Vector.Algorithms.Quicksort.Predefined.UTupleSequentialMedian3or5IO
import Data.Vector.Algorithms.Quicksort.Predefined.UTupleParallelStrategiesMedian3or5ST
import Data.Vector.Algorithms.Quicksort.Predefined.UTupleParallelStrategiesMedian3or5IO

tests :: TestTree
tests = testGroup "Data.Vector.Algorithms.Quicksort tests"
  [ sortProps
  , sortTestsST
  , sortTestsIO
  , sortTestsSTtoIO
  ]

numTests :: Int
numTests = 100_000

{-# INLINE runSort #-}
runSort
  :: (G.Vector w a, v ~ G.Mutable w)
  => (v s a -> ST s ())
  -> [a]
  -> ST s [a]
runSort doSort xs = do
  (ys :: v s a) <- G.unsafeThaw $ G.fromList xs
  doSort ys
  G.toList <$> G.unsafeFreeze ys

{-# INLINE runSortIO #-}
runSortIO
  :: (G.Vector w a, v ~ G.Mutable w)
  => (v RealWorld a -> IO ())
  -> [a]
  -> IO [a]
runSortIO doSort xs = do
  (ys :: v RealWorld a) <- G.unsafeThaw $ G.fromList xs
  doSort ys
  G.toList <$> G.unsafeFreeze ys

{-# INLINE runSortSTtoIO #-}
runSortSTtoIO
  :: (G.Vector w a, v ~ G.Mutable w)
  => (forall s. v s a -> ST s ())
  -> [a]
  -> IO [a]
runSortSTtoIO doSort xs = do
  (ys :: v RealWorld a) <- G.unsafeThaw $ G.fromList xs
  stToIO $ doSort ys
  G.toList <$> G.unsafeFreeze ys

sortProps :: TestTree
sortProps = localOption (QC.QuickCheckTests numTests) $ testGroup "sort properties"
  [ testGroup "ST"
    [ QC.testProperty "Data.Vector (TestPair Int32 Int32) sorting Sequential Median3" $
      sortsAndDoesNotLoseItemsST @V.Vector sortVPairSequentialMedian3ST
    , QC.testProperty "Data.Vector (TestPair Int32 Int32) sorting Sequential Median3or5" $
      sortsAndDoesNotLoseItemsST @V.Vector sortVPairSequentialMedian3or5ST
    , QC.testProperty "Data.Vector (TestPair Int32 Int32) sorting ParallelStrategies Median3" $
      sortsAndDoesNotLoseItemsST @V.Vector sortVPairParallelStrategiesMedian3ST
    , QC.testProperty "Data.Vector (TestPair Int32 Int32) sorting ParallelStrategies Median3or5" $
      sortsAndDoesNotLoseItemsST @V.Vector sortVPairParallelStrategiesMedian3or5ST

    , QC.testProperty "Data.Vector.Unboxed (TestPair Int32 Int32) sorting Sequential Median3" $
      sortsAndDoesNotLoseItemsST @U.Vector sortUPairSequentialMedian3ST
    , QC.testProperty "Data.Vector.Unboxed (TestPair Int32 Int32) sorting Sequential Median3or5" $
      sortsAndDoesNotLoseItemsST @U.Vector sortUPairSequentialMedian3or5ST
    , QC.testProperty "Data.Vector.Unboxed (TestPair Int32 Int32) sorting ParallelStrategies Median3" $
      sortsAndDoesNotLoseItemsST @U.Vector sortUPairParallelStrategiesMedian3ST
    , QC.testProperty "Data.Vector.Unboxed (TestPair Int32 Int32) sorting ParallelStrategies Median3or5" $
      sortsAndDoesNotLoseItemsST @U.Vector sortUPairParallelStrategiesMedian3or5ST
    ]
  , testGroup "IO"
    [ QC.testProperty "Data.Vector (TestPair Int32 Int32) sorting Sequential Median3" $
      sortsAndDoesNotLoseItemsIO @V.Vector sortVPairSequentialMedian3IO
    , QC.testProperty "Data.Vector (TestPair Int32 Int32) sorting Sequential Median3or5" $
      sortsAndDoesNotLoseItemsIO @V.Vector sortVPairSequentialMedian3or5IO
    , QC.testProperty "Data.Vector (TestPair Int32 Int32) sorting ParallelStrategies Median3" $
      sortsAndDoesNotLoseItemsIO @V.Vector sortVPairParallelStrategiesMedian3IO
    , QC.testProperty "Data.Vector (TestPair Int32 Int32) sorting ParallelStrategies Median3or5" $
      sortsAndDoesNotLoseItemsIO @V.Vector sortVPairParallelStrategiesMedian3or5IO

    , QC.testProperty "Data.Vector.Unboxed (TestPair Int32 Int32) sorting Sequential Median3" $
      sortsAndDoesNotLoseItemsIO @U.Vector sortUPairSequentialMedian3IO
    , QC.testProperty "Data.Vector.Unboxed (TestPair Int32 Int32) sorting Sequential Median3or5" $
      sortsAndDoesNotLoseItemsIO @U.Vector sortUPairSequentialMedian3or5IO
    , QC.testProperty "Data.Vector.Unboxed (TestPair Int32 Int32) sorting ParallelStrategies Median3" $
      sortsAndDoesNotLoseItemsIO @U.Vector sortUPairParallelStrategiesMedian3IO
    , QC.testProperty "Data.Vector.Unboxed (TestPair Int32 Int32) sorting ParallelStrategies Median3or5" $
      sortsAndDoesNotLoseItemsIO @U.Vector sortUPairParallelStrategiesMedian3or5IO
    ]
  , testGroup "IO via stToIO"
    [ QC.testProperty "Data.Vector (TestPair Int32 Int32) sorting Sequential Median3" $
      sortsAndDoesNotLoseItemsSTtoIO @V.Vector sortVPairSequentialMedian3ST
    , QC.testProperty "Data.Vector (TestPair Int32 Int32) sorting Sequential Median3or5" $
      sortsAndDoesNotLoseItemsSTtoIO @V.Vector sortVPairSequentialMedian3or5ST
    , QC.testProperty "Data.Vector (TestPair Int32 Int32) sorting ParallelStrategies Median3" $
      sortsAndDoesNotLoseItemsSTtoIO @V.Vector sortVPairParallelStrategiesMedian3ST
    , QC.testProperty "Data.Vector (TestPair Int32 Int32) sorting ParallelStrategies Median3or5" $
      sortsAndDoesNotLoseItemsSTtoIO @V.Vector sortVPairParallelStrategiesMedian3or5ST

    , QC.testProperty "Data.Vector.Unboxed (TestPair Int32 Int32) sorting Sequential Median3" $
      sortsAndDoesNotLoseItemsSTtoIO @U.Vector sortUPairSequentialMedian3ST
    , QC.testProperty "Data.Vector.Unboxed (TestPair Int32 Int32) sorting Sequential Median3or5" $
      sortsAndDoesNotLoseItemsSTtoIO @U.Vector sortUPairSequentialMedian3or5ST
    , QC.testProperty "Data.Vector.Unboxed (TestPair Int32 Int32) sorting ParallelStrategies Median3" $
      sortsAndDoesNotLoseItemsSTtoIO @U.Vector sortUPairParallelStrategiesMedian3ST
    , QC.testProperty "Data.Vector.Unboxed (TestPair Int32 Int32) sorting ParallelStrategies Median3or5" $
      sortsAndDoesNotLoseItemsSTtoIO @U.Vector sortUPairParallelStrategiesMedian3or5ST
    ]
  ]
  where
    sortsAndDoesNotLoseItemsST
      :: forall w v. (G.Vector w (TestPair Int32 Int32), v ~ G.Mutable w)
      => (forall s. G.Mutable w s (TestPair Int32 Int32) -> ST s ())
      -> [Int32]
      -> QC.Property
    sortsAndDoesNotLoseItemsST doSort xs =
      ((QC.===) `on` S.fromList . map (snd . toTuple)) sorted unsorted
      QC..&&.
      ((QC.===) `on` map (fst . toTuple)) sorted (L.sort unsorted)
      where
        sorted   = runST $ runSort @w doSort unsorted
        unsorted = zipWith TestPair xs [0..]

    sortsAndDoesNotLoseItemsIO
      :: forall w v. (G.Vector w (TestPair Int32 Int32), v ~ G.Mutable w)
      => (G.Mutable w RealWorld (TestPair Int32 Int32) -> IO ())
      -> [Int32]
      -> QC.Property
    sortsAndDoesNotLoseItemsIO doSort xs = QC.ioProperty $ do
      sorted <- runSortIO @w doSort unsorted
      pure $
        ((QC.===) `on` S.fromList . map (snd . toTuple)) sorted unsorted
        QC..&&.
        ((QC.===) `on` map (fst . toTuple)) sorted (L.sort unsorted)
      where
        unsorted = zipWith TestPair xs [0..]

    sortsAndDoesNotLoseItemsSTtoIO
      :: forall w v. (G.Vector w (TestPair Int32 Int32), v ~ G.Mutable w)
      => (forall s. G.Mutable w s (TestPair Int32 Int32) -> ST s ())
      -> [Int32]
      -> QC.Property
    sortsAndDoesNotLoseItemsSTtoIO doSort xs = QC.ioProperty $ do
      sorted <- runSortSTtoIO @w doSort unsorted
      pure $
        ((QC.===) `on` S.fromList . map (snd . toTuple)) sorted unsorted
        QC..&&.
        ((QC.===) `on` map (fst . toTuple)) sorted (L.sort unsorted)
      where
        unsorted = zipWith TestPair xs [0..]

sortTestsST :: TestTree
sortTestsST = localOption (QC.QuickCheckTests numTests) $ testGroup "sort tests in ST"
  [ QC.testProperty "Data.Vector Int64 sorting Sequential Median3" $
      \(xs :: [Int64]) ->
        runST (runSort @V.Vector sortVIntSequentialMedian3ST xs) == L.sort xs
  , QC.testProperty "Data.Vector (Int32, Int32) sorting Sequential Median3" $
      \(xs :: [(Int32, Int32)]) ->
        runST (runSort @V.Vector sortVTupleSequentialMedian3ST xs) == L.sort xs
  , QC.testProperty "Data.Vector.Unboxed Int64 sorting Sequential Median3" $
      \(xs :: [Int64]) ->
        runST (runSort @U.Vector sortUIntSequentialMedian3ST xs) == L.sort xs
  , QC.testProperty "Data.Vector.Unboxed (Int32, Int32) sorting Sequential Median3" $
      \(xs :: [(Int32, Int32)]) ->
        runST (runSort @U.Vector sortUTupleSequentialMedian3ST xs) == L.sort xs

  , QC.testProperty "Data.Vector Int64 sorting Sequential Median3or5" $
      \(xs :: [Int64]) ->
        runST (runSort @V.Vector sortVIntSequentialMedian3or5ST xs) == L.sort xs
  , QC.testProperty "Data.Vector (Int32, Int32) sorting Sequential Median3or5" $
      \(xs :: [(Int32, Int32)]) ->
        runST (runSort @V.Vector sortVTupleSequentialMedian3or5ST xs) == L.sort xs
  , QC.testProperty "Data.Vector.Unboxed Int64 sorting Sequential Median3or5" $
      \(xs :: [Int64]) ->
        runST (runSort @U.Vector sortUIntSequentialMedian3or5ST xs) == L.sort xs
  , QC.testProperty "Data.Vector.Unboxed (Int32, Int32) sorting Sequential Median3or5" $
      \(xs :: [(Int32, Int32)]) ->
        runST (runSort @U.Vector sortUTupleSequentialMedian3or5ST xs) == L.sort xs

  , QC.testProperty "Data.Vector Int64 sorting ParStrategies Median3or5" $
      \(xs :: [Int64]) ->
        runST (runSort @V.Vector sortVIntParallelStrategiesMedian3or5ST xs) == L.sort xs
  , QC.testProperty "Data.Vector (Int32, Int32) sorting ParStrategies Median3or5" $
      \(xs :: [(Int32, Int32)]) ->
        runST (runSort @V.Vector sortVTupleParallelStrategiesMedian3or5ST xs) == L.sort xs
  , QC.testProperty "Data.Vector.Unboxed Int64 sorting ParStrategies Median3or5" $
      \(xs :: [Int64]) ->
        runST (runSort @U.Vector sortUIntParallelStrategiesMedian3or5ST xs) == L.sort xs
  , QC.testProperty "Data.Vector.Unboxed (Int32, Int32) sorting ParStrategies Median3or5" $
      \(xs :: [(Int32, Int32)]) ->
        runST (runSort @U.Vector sortUTupleParallelStrategiesMedian3or5ST xs) == L.sort xs
  ]

sortTestsIO :: TestTree
sortTestsIO = localOption (QC.QuickCheckTests numTests) $ testGroup "sort tests in IO"
  [ QC.testProperty "Data.Vector Int64 sorting Sequential Median3" $
      \(xs :: [Int64]) -> QC.ioProperty $ do
        ys <- runSortIO @V.Vector sortVIntSequentialMedian3IO xs
        pure $ ys == L.sort xs
  , QC.testProperty "Data.Vector (Int32, Int32) sorting Sequential Median3" $
      \(xs :: [(Int32, Int32)]) -> QC.ioProperty $ do
        ys <- runSortIO @V.Vector sortVTupleSequentialMedian3IO xs
        pure $ ys == L.sort xs
  , QC.testProperty "Data.Vector.Unboxed Int64 sorting Sequential Median3" $
      \(xs :: [Int64]) -> QC.ioProperty $ do
        ys <- runSortIO @U.Vector sortUIntSequentialMedian3IO xs
        pure $ ys == L.sort xs
  , QC.testProperty "Data.Vector.Unboxed (Int32, Int32) sorting Sequential Median3" $
      \(xs :: [(Int32, Int32)]) -> QC.ioProperty $ do
        ys <- runSortIO @U.Vector sortUTupleSequentialMedian3IO xs
        pure $ ys == L.sort xs

  , QC.testProperty "Data.Vector Int64 sorting Sequential Median3or5" $
      \(xs :: [Int64]) -> QC.ioProperty $ do
        ys <- runSortIO @V.Vector sortVIntSequentialMedian3or5IO xs
        pure $ ys == L.sort xs
  , QC.testProperty "Data.Vector (Int32, Int32) sorting Sequential Median3or5" $
      \(xs :: [(Int32, Int32)]) -> QC.ioProperty $ do
        ys <- runSortIO @V.Vector sortVTupleSequentialMedian3or5IO xs
        pure $ ys == L.sort xs
  , QC.testProperty "Data.Vector.Unboxed Int64 sorting Sequential Median3or5" $
      \(xs :: [Int64]) -> QC.ioProperty $ do
        ys <- runSortIO @U.Vector sortUIntSequentialMedian3or5IO xs
        pure $ ys == L.sort xs
  , QC.testProperty "Data.Vector.Unboxed (Int32, Int32) sorting Sequential Median3or5" $
      \(xs :: [(Int32, Int32)]) -> QC.ioProperty $ do
        ys <- runSortIO @U.Vector sortUTupleSequentialMedian3or5IO xs
        pure $ ys == L.sort xs

  , QC.testProperty "Data.Vector Int64 sorting ParStrategies Median3or5" $
      \(xs :: [Int64]) -> QC.ioProperty $ do
        ys <- runSortIO @V.Vector sortVIntParallelStrategiesMedian3or5IO xs
        pure $ ys == L.sort xs
  , QC.testProperty "Data.Vector (Int32, Int32) sorting ParStrategies Median3or5" $
      \(xs :: [(Int32, Int32)]) -> QC.ioProperty $ do
        ys <- runSortIO @V.Vector sortVTupleParallelStrategiesMedian3or5IO xs
        pure $ ys == L.sort xs
  , QC.testProperty "Data.Vector.Unboxed Int64 sorting ParStrategies Median3or5" $
      \(xs :: [Int64]) -> QC.ioProperty $ do
        ys <- runSortIO @U.Vector sortUIntParallelStrategiesMedian3or5IO xs
        pure $ ys == L.sort xs
  , QC.testProperty "Data.Vector.Unboxed (Int32, Int32) sorting ParStrategies Median3or5" $
      \(xs :: [(Int32, Int32)]) -> QC.ioProperty $ do
        ys <- runSortIO @U.Vector sortUTupleParallelStrategiesMedian3or5IO xs
        pure $ ys == L.sort xs

  , QC.testProperty "Data.Vector.Primitive Int64 sorting Parallel Median3" $
      \(xs :: [Int64]) -> QC.ioProperty $ do
        ys <- runSortIO @P.Vector sortPIntParallelMedian3IO xs
        pure $ ys == L.sort xs
  , QC.testProperty "Data.Vector.Primitive Int64 sorting Parallel Median3or5" $
      \(xs :: [Int64]) -> QC.ioProperty $ do
        ys <- runSortIO @P.Vector sortPIntParallelMedian3or5IO xs
        pure $ ys == L.sort xs
  ]

sortTestsSTtoIO :: TestTree
sortTestsSTtoIO = localOption (QC.QuickCheckTests numTests) $ testGroup "sort tests in IO via stToIO"
  [ QC.testProperty "Data.Vector Int64 sorting Sequential Median3" $
      \(xs :: [Int64]) -> QC.ioProperty $ do
        ys <- runSortSTtoIO @V.Vector sortVIntSequentialMedian3ST xs
        pure $ ys == L.sort xs
  , QC.testProperty "Data.Vector (Int32, Int32) sorting Sequential Median3" $
      \(xs :: [(Int32, Int32)]) -> QC.ioProperty $ do
        ys <- runSortSTtoIO @V.Vector sortVTupleSequentialMedian3ST xs
        pure $ ys == L.sort xs
  , QC.testProperty "Data.Vector.Unboxed Int64 sorting Sequential Median3" $
      \(xs :: [Int64]) -> QC.ioProperty $ do
        ys <- runSortSTtoIO @U.Vector sortUIntSequentialMedian3ST xs
        pure $ ys == L.sort xs
  , QC.testProperty "Data.Vector.Unboxed (Int32, Int32) sorting Sequential Median3" $
      \(xs :: [(Int32, Int32)]) -> QC.ioProperty $ do
        ys <- runSortSTtoIO @U.Vector sortUTupleSequentialMedian3ST xs
        pure $ ys == L.sort xs

  , QC.testProperty "Data.Vector Int64 sorting Sequential Median3or5" $
      \(xs :: [Int64]) -> QC.ioProperty $ do
        ys <- runSortSTtoIO @V.Vector sortVIntSequentialMedian3or5ST xs
        pure $ ys == L.sort xs
  , QC.testProperty "Data.Vector (Int32, Int32) sorting Sequential Median3or5" $
      \(xs :: [(Int32, Int32)]) -> QC.ioProperty $ do
        ys <- runSortSTtoIO @V.Vector sortVTupleSequentialMedian3or5ST xs
        pure $ ys == L.sort xs
  , QC.testProperty "Data.Vector.Unboxed Int64 sorting Sequential Median3or5" $
      \(xs :: [Int64]) -> QC.ioProperty $ do
        ys <- runSortSTtoIO @U.Vector sortUIntSequentialMedian3or5ST xs
        pure $ ys == L.sort xs
  , QC.testProperty "Data.Vector.Unboxed (Int32, Int32) sorting Sequential Median3or5" $
      \(xs :: [(Int32, Int32)]) -> QC.ioProperty $ do
        ys <- runSortSTtoIO @U.Vector sortUTupleSequentialMedian3or5ST xs
        pure $ ys == L.sort xs

  , QC.testProperty "Data.Vector Int64 sorting ParStrategies Median3or5" $
      \(xs :: [Int64]) -> QC.ioProperty $ do
        ys <- runSortSTtoIO @V.Vector sortVIntParallelStrategiesMedian3or5ST xs
        pure $ ys == L.sort xs
  , QC.testProperty "Data.Vector (Int32, Int32) sorting ParStrategies Median3or5" $
      \(xs :: [(Int32, Int32)]) -> QC.ioProperty $ do
        ys <- runSortSTtoIO @V.Vector sortVTupleParallelStrategiesMedian3or5ST xs
        pure $ ys == L.sort xs
  , QC.testProperty "Data.Vector.Unboxed Int64 sorting ParStrategies Median3or5" $
      \(xs :: [Int64]) -> QC.ioProperty $ do
        ys <- runSortSTtoIO @U.Vector sortUIntParallelStrategiesMedian3or5ST xs
        pure $ ys == L.sort xs
  , QC.testProperty "Data.Vector.Unboxed (Int32, Int32) sorting ParStrategies Median3or5" $
      \(xs :: [(Int32, Int32)]) -> QC.ioProperty $ do
        ys <- runSortSTtoIO @U.Vector sortUTupleParallelStrategiesMedian3or5ST xs
        pure $ ys == L.sort xs
  ]
