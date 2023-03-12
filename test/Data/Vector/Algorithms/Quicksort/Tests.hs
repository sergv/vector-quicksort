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
import Data.Int
import Data.List qualified as L
import Data.Vector qualified as V
import Data.Vector.Generic qualified as G
import Data.Vector.Unboxed qualified as U
import Test.Tasty
import Test.Tasty.QuickCheck qualified as QC

-- import Data.Vector.Generic.Mutable qualified as GM

import Data.Vector.Algorithms.Quicksort.Predefined.VIntSequentialMedian3ST
import Data.Vector.Algorithms.Quicksort.Predefined.VIntSequentialMedian3IO
import Data.Vector.Algorithms.Quicksort.Predefined.VPairSequentialMedian3ST
import Data.Vector.Algorithms.Quicksort.Predefined.VPairSequentialMedian3IO
-- import Data.Vector.Algorithms.Quicksort.Predefined.VIntParallelStrategiesMedian3ST
-- import Data.Vector.Algorithms.Quicksort.Predefined.VIntParallelStrategiesMedian3IO
-- import Data.Vector.Algorithms.Quicksort.Predefined.VPairParallelStrategiesMedian3ST
-- import Data.Vector.Algorithms.Quicksort.Predefined.VPairParallelStrategiesMedian3IO

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
-- import Data.Vector.Algorithms.Quicksort.Predefined.UPairParallelStrategiesMedian3ST
-- import Data.Vector.Algorithms.Quicksort.Predefined.UPairParallelStrategiesMedian3IO

import Data.Vector.Algorithms.Quicksort.Predefined.UIntSequentialMedian3or5ST
import Data.Vector.Algorithms.Quicksort.Predefined.UIntSequentialMedian3or5IO
import Data.Vector.Algorithms.Quicksort.Predefined.UPairSequentialMedian3or5ST
import Data.Vector.Algorithms.Quicksort.Predefined.UPairSequentialMedian3or5IO
import Data.Vector.Algorithms.Quicksort.Predefined.UIntParallelStrategiesMedian3or5ST
import Data.Vector.Algorithms.Quicksort.Predefined.UIntParallelStrategiesMedian3or5IO
import Data.Vector.Algorithms.Quicksort.Predefined.UPairParallelStrategiesMedian3or5ST
import Data.Vector.Algorithms.Quicksort.Predefined.UPairParallelStrategiesMedian3or5IO



tests :: TestTree
tests = testGroup "Data.Vector.Algorithms.Quicksort tests"
  [ sortTestsST
  , sortTestsIO
  , sortTestsSTtoIO
  ]

numTests :: Int
numTests = 100_000

-- runSort
--   :: forall v a s. (G.Vector v a, GM.MVector (G.Mutable v) a)
--   => (G.Mutable v s a -> ST s ())
--   -> [a]
--   -> ST s [a]
-- runSort doSort xs = do
--   ys <- G.unsafeThaw $ G.fromList xs
--   doSort ys
--   G.toList <$> G.unsafeFreeze ys

runSort
  :: forall w v a s. (G.Vector w a, v ~ G.Mutable w)
  => (v s a -> ST s ())
  -> [a]
  -> ST s [a]
runSort doSort xs = do
  (ys :: v s a) <- G.unsafeThaw $ G.fromList xs
  doSort ys
  G.toList <$> G.unsafeFreeze ys

runSortIO
  :: forall w v a. (G.Vector w a, v ~ G.Mutable w)
  => (v RealWorld a -> IO ())
  -> [a]
  -> IO [a]
runSortIO doSort xs = do
  (ys :: v RealWorld a) <- G.unsafeThaw $ G.fromList xs
  doSort ys
  G.toList <$> G.unsafeFreeze ys

runSortSTtoIO
  :: forall w v a. (G.Vector w a, v ~ G.Mutable w)
  => (forall s. v s a -> ST s ())
  -> [a]
  -> IO [a]
runSortSTtoIO doSort xs = do
  (ys :: v RealWorld a) <- G.unsafeThaw $ G.fromList xs
  stToIO $ doSort ys
  G.toList <$> G.unsafeFreeze ys


sortTestsST :: TestTree
sortTestsST = localOption (QC.QuickCheckTests numTests) $ testGroup "sort tests in ST"
  [ QC.testProperty "Data.Vector Int64 sorting Sequential Median3" $
      \(xs :: [Int64]) ->
        runST (runSort @V.Vector sortVIntSequentialMedian3ST xs) == L.sort xs
  , QC.testProperty "Data.Vector (Int32, Int32) sorting Sequential Median3" $
      \(xs :: [(Int32, Int32)]) ->
        runST (runSort @V.Vector sortVPairSequentialMedian3ST xs) == L.sort xs
  , QC.testProperty "Data.Vector.Unboxed Int64 sorting Sequential Median3" $
      \(xs :: [Int64]) ->
        runST (runSort @U.Vector sortUIntSequentialMedian3ST xs) == L.sort xs
  , QC.testProperty "Data.Vector.Unboxed (Int32, Int32) sorting Sequential Median3" $
      \(xs :: [(Int32, Int32)]) ->
        runST (runSort @U.Vector sortUPairSequentialMedian3ST xs) == L.sort xs

  , QC.testProperty "Data.Vector Int64 sorting Sequential Median3or5" $
      \(xs :: [Int64]) ->
        runST (runSort @V.Vector sortVIntSequentialMedian3or5ST xs) == L.sort xs
  , QC.testProperty "Data.Vector (Int32, Int32) sorting Sequential Median3or5" $
      \(xs :: [(Int32, Int32)]) ->
        runST (runSort @V.Vector sortVPairSequentialMedian3or5ST xs) == L.sort xs
  , QC.testProperty "Data.Vector.Unboxed Int64 sorting Sequential Median3or5" $
      \(xs :: [Int64]) ->
        runST (runSort @U.Vector sortUIntSequentialMedian3or5ST xs) == L.sort xs
  , QC.testProperty "Data.Vector.Unboxed (Int32, Int32) sorting Sequential Median3or5" $
      \(xs :: [(Int32, Int32)]) ->
        runST (runSort @U.Vector sortUPairSequentialMedian3or5ST xs) == L.sort xs

  , QC.testProperty "Data.Vector Int64 sorting ParStrategies Median3or5" $
      \(xs :: [Int64]) ->
        runST (runSort @V.Vector sortVIntParallelStrategiesMedian3or5ST xs) == L.sort xs
  , QC.testProperty "Data.Vector (Int32, Int32) sorting ParStrategies Median3or5" $
      \(xs :: [(Int32, Int32)]) ->
        runST (runSort @V.Vector sortVPairParallelStrategiesMedian3or5ST xs) == L.sort xs
  , QC.testProperty "Data.Vector.Unboxed Int64 sorting ParStrategies Median3or5" $
      \(xs :: [Int64]) ->
        runST (runSort @U.Vector sortUIntParallelStrategiesMedian3or5ST xs) == L.sort xs
  , QC.testProperty "Data.Vector.Unboxed (Int32, Int32) sorting ParStrategies Median3or5" $
      \(xs :: [(Int32, Int32)]) ->
        runST (runSort @U.Vector sortUPairParallelStrategiesMedian3or5ST xs) == L.sort xs
  ]

sortTestsIO :: TestTree
sortTestsIO = localOption (QC.QuickCheckTests numTests) $ testGroup "sort tests in IO"
  [ QC.testProperty "Data.Vector Int64 sorting Sequential Median3" $
      \(xs :: [Int64]) -> QC.ioProperty $ do
        ys <- runSortIO @V.Vector sortVIntSequentialMedian3IO xs
        pure $ ys == L.sort xs
  , QC.testProperty "Data.Vector (Int32, Int32) sorting Sequential Median3" $
      \(xs :: [(Int32, Int32)]) -> QC.ioProperty $ do
        ys <- runSortIO @V.Vector sortVPairSequentialMedian3IO xs
        pure $ ys == L.sort xs
  , QC.testProperty "Data.Vector.Unboxed Int64 sorting Sequential Median3" $
      \(xs :: [Int64]) -> QC.ioProperty $ do
        ys <- runSortIO @U.Vector sortUIntSequentialMedian3IO xs
        pure $ ys == L.sort xs
  , QC.testProperty "Data.Vector.Unboxed (Int32, Int32) sorting Sequential Median3" $
      \(xs :: [(Int32, Int32)]) -> QC.ioProperty $ do
        ys <- runSortIO @U.Vector sortUPairSequentialMedian3IO xs
        pure $ ys == L.sort xs

  , QC.testProperty "Data.Vector Int64 sorting Sequential Median3or5" $
      \(xs :: [Int64]) -> QC.ioProperty $ do
        ys <- runSortIO @V.Vector sortVIntSequentialMedian3or5IO xs
        pure $ ys == L.sort xs
  , QC.testProperty "Data.Vector (Int32, Int32) sorting Sequential Median3or5" $
      \(xs :: [(Int32, Int32)]) -> QC.ioProperty $ do
        ys <- runSortIO @V.Vector sortVPairSequentialMedian3or5IO xs
        pure $ ys == L.sort xs
  , QC.testProperty "Data.Vector.Unboxed Int64 sorting Sequential Median3or5" $
      \(xs :: [Int64]) -> QC.ioProperty $ do
        ys <- runSortIO @U.Vector sortUIntSequentialMedian3or5IO xs
        pure $ ys == L.sort xs
  , QC.testProperty "Data.Vector.Unboxed (Int32, Int32) sorting Sequential Median3or5" $
      \(xs :: [(Int32, Int32)]) -> QC.ioProperty $ do
        ys <- runSortIO @U.Vector sortUPairSequentialMedian3or5IO xs
        pure $ ys == L.sort xs

  , QC.testProperty "Data.Vector Int64 sorting ParStrategies Median3or5" $
      \(xs :: [Int64]) -> QC.ioProperty $ do
        ys <- runSortIO @V.Vector sortVIntParallelStrategiesMedian3or5IO xs
        pure $ ys == L.sort xs
  , QC.testProperty "Data.Vector (Int32, Int32) sorting ParStrategies Median3or5" $
      \(xs :: [(Int32, Int32)]) -> QC.ioProperty $ do
        ys <- runSortIO @V.Vector sortVPairParallelStrategiesMedian3or5IO xs
        pure $ ys == L.sort xs
  , QC.testProperty "Data.Vector.Unboxed Int64 sorting ParStrategies Median3or5" $
      \(xs :: [Int64]) -> QC.ioProperty $ do
        ys <- runSortIO @U.Vector sortUIntParallelStrategiesMedian3or5IO xs
        pure $ ys == L.sort xs
  , QC.testProperty "Data.Vector.Unboxed (Int32, Int32) sorting ParStrategies Median3or5" $
      \(xs :: [(Int32, Int32)]) -> QC.ioProperty $ do
        ys <- runSortIO @U.Vector sortUPairParallelStrategiesMedian3or5IO xs
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
        ys <- runSortSTtoIO @V.Vector sortVPairSequentialMedian3ST xs
        pure $ ys == L.sort xs
  , QC.testProperty "Data.Vector.Unboxed Int64 sorting Sequential Median3" $
      \(xs :: [Int64]) -> QC.ioProperty $ do
        ys <- runSortSTtoIO @U.Vector sortUIntSequentialMedian3ST xs
        pure $ ys == L.sort xs
  , QC.testProperty "Data.Vector.Unboxed (Int32, Int32) sorting Sequential Median3" $
      \(xs :: [(Int32, Int32)]) -> QC.ioProperty $ do
        ys <- runSortSTtoIO @U.Vector sortUPairSequentialMedian3ST xs
        pure $ ys == L.sort xs

  , QC.testProperty "Data.Vector Int64 sorting Sequential Median3or5" $
      \(xs :: [Int64]) -> QC.ioProperty $ do
        ys <- runSortSTtoIO @V.Vector sortVIntSequentialMedian3or5ST xs
        pure $ ys == L.sort xs
  , QC.testProperty "Data.Vector (Int32, Int32) sorting Sequential Median3or5" $
      \(xs :: [(Int32, Int32)]) -> QC.ioProperty $ do
        ys <- runSortSTtoIO @V.Vector sortVPairSequentialMedian3or5ST xs
        pure $ ys == L.sort xs
  , QC.testProperty "Data.Vector.Unboxed Int64 sorting Sequential Median3or5" $
      \(xs :: [Int64]) -> QC.ioProperty $ do
        ys <- runSortSTtoIO @U.Vector sortUIntSequentialMedian3or5ST xs
        pure $ ys == L.sort xs
  , QC.testProperty "Data.Vector.Unboxed (Int32, Int32) sorting Sequential Median3or5" $
      \(xs :: [(Int32, Int32)]) -> QC.ioProperty $ do
        ys <- runSortSTtoIO @U.Vector sortUPairSequentialMedian3or5ST xs
        pure $ ys == L.sort xs

  , QC.testProperty "Data.Vector Int64 sorting ParStrategies Median3or5" $
      \(xs :: [Int64]) -> QC.ioProperty $ do
        ys <- runSortSTtoIO @V.Vector sortVIntParallelStrategiesMedian3or5ST xs
        pure $ ys == L.sort xs
  , QC.testProperty "Data.Vector (Int32, Int32) sorting ParStrategies Median3or5" $
      \(xs :: [(Int32, Int32)]) -> QC.ioProperty $ do
        ys <- runSortSTtoIO @V.Vector sortVPairParallelStrategiesMedian3or5ST xs
        pure $ ys == L.sort xs
  , QC.testProperty "Data.Vector.Unboxed Int64 sorting ParStrategies Median3or5" $
      \(xs :: [Int64]) -> QC.ioProperty $ do
        ys <- runSortSTtoIO @U.Vector sortUIntParallelStrategiesMedian3or5ST xs
        pure $ ys == L.sort xs
  , QC.testProperty "Data.Vector.Unboxed (Int32, Int32) sorting ParStrategies Median3or5" $
      \(xs :: [(Int32, Int32)]) -> QC.ioProperty $ do
        ys <- runSortSTtoIO @U.Vector sortUPairParallelStrategiesMedian3or5ST xs
        pure $ ys == L.sort xs
  ]
