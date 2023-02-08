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
import Data.List qualified as L
import Data.Vector qualified as V
import Data.Vector.Generic qualified as G
import Data.Vector.Unboxed qualified as U
import Test.Tasty
import Test.Tasty.QuickCheck qualified as QC

-- import Data.Vector.Generic.Mutable qualified as GM

import Data.Vector.Algorithms.Quicksort qualified as Quick
import Data.Vector.Algorithms.Quicksort.Fork
import Data.Vector.Algorithms.Quicksort.Median

tests :: TestTree
tests = testGroup "Data.Vector.Algorithms.Quicksort tests"
  [ sortTests
  ]

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

sortTests :: TestTree
sortTests = localOption (QC.QuickCheckTests 1_000_000) $ testGroup "sort tests"
  [ QC.testProperty "Data.Vector Int sorting Sequential Median3" $
      \(xs :: [Int]) ->
        runST (runSort @V.Vector (Quick.sort Sequential (Median3 @Int)) xs) == L.sort xs
  , QC.testProperty "Data.Vector (Int, Int) sorting Sequential Median3" $
      \(xs :: [(Int, Int)]) ->
        runST (runSort @V.Vector (Quick.sort Sequential (Median3 @(Int, Int))) xs) == L.sort xs
  , QC.testProperty "Data.Vector.Unboxed Int sorting Sequential Median3" $
      \(xs :: [Int]) ->
        runST (runSort @U.Vector (Quick.sort Sequential (Median3 @Int)) xs) == L.sort xs
  , QC.testProperty "Data.Vector.Unboxed (Int, Int) sorting Sequential Median3" $
      \(xs :: [(Int, Int)]) ->
        runST (runSort @U.Vector (Quick.sort Sequential (Median3 @(Int, Int))) xs) == L.sort xs

  , QC.testProperty "Data.Vector Int sorting Sequential Median3or5" $
      \(xs :: [Int]) ->
        runST (runSort @V.Vector (Quick.sort Sequential (Median3or5 @Int)) xs) == L.sort xs
  , QC.testProperty "Data.Vector (Int, Int) sorting Sequential Median3or5" $
      \(xs :: [(Int, Int)]) ->
        runST (runSort @V.Vector (Quick.sort Sequential (Median3or5 @(Int, Int))) xs) == L.sort xs
  , QC.testProperty "Data.Vector.Unboxed Int sorting Sequential Median3or5" $
      \(xs :: [Int]) ->
        runST (runSort @U.Vector (Quick.sort Sequential (Median3or5 @Int)) xs) == L.sort xs
  , QC.testProperty "Data.Vector.Unboxed (Int, Int) sorting Sequential Median3or5" $
      \(xs :: [(Int, Int)]) ->
        runST (runSort @U.Vector (Quick.sort Sequential (Median3or5 @(Int, Int))) xs) == L.sort xs

  , QC.testProperty "Data.Vector Int sorting ParStrategies Median3or5" $
      \(xs :: [Int]) ->
        runST (runSort @V.Vector (Quick.sort ParStrategies (Median3or5 @Int)) xs) == L.sort xs
  , QC.testProperty "Data.Vector (Int, Int) sorting ParStrategies Median3or5" $
      \(xs :: [(Int, Int)]) ->
        runST (runSort @V.Vector (Quick.sort ParStrategies (Median3or5 @(Int, Int))) xs) == L.sort xs
  , QC.testProperty "Data.Vector.Unboxed Int sorting ParStrategies Median3or5" $
      \(xs :: [Int]) ->
        runST (runSort @U.Vector (Quick.sort ParStrategies (Median3or5 @Int)) xs) == L.sort xs
  , QC.testProperty "Data.Vector.Unboxed (Int, Int) sorting ParStrategies Median3or5" $
      \(xs :: [(Int, Int)]) ->
        runST (runSort @U.Vector (Quick.sort ParStrategies (Median3or5 @(Int, Int))) xs) == L.sort xs
  ]


