----------------------------------------------------------------------------
-- |
-- Module      :  QuicksortRunner
-- Copyright   :  (c) Sergey Vinokurov 2023
-- License     :  Apache-2.0 (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

{-# LANGUAGE ApplicativeDo     #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module QuicksortRunner (main) where

import Codec.Serialise qualified as CBOR
import Control.Concurrent
import Control.Concurrent.Async
import Control.DeepSeq
import Control.Exception
import Control.Monad
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as C8
import Data.ByteString.Lazy qualified as BSL
import Data.Int
import Data.List qualified as L
import Data.Vector.Primitive qualified as P
import Debug.Trace
import Options.Applicative hiding (str, action)
import Prettyprinter
import Prettyprinter.Combinators
import System.Clock.Seconds
import System.FilePath
import Text.Printf

import Data.Coerce
import Data.Vector.Generic qualified as G

import Data.Vector.Algorithms.Quicksort as Quick
import Data.Vector.Algorithms.Quicksort.Fork2 as Quick
import Data.Vector.Algorithms.Quicksort.Median as Quick
import Data.Vector.Algorithms.Quicksort.Parameterised qualified as Quick
import Data.Vector.Algorithms.Quicksort.Predefined.AveragingMedian

-- import Data.Vector.Algorithms.Quicksort.Predefined.PIntParallelMedian3or5IO

newtype Config = Config
  { cfgInputFile :: FilePath
  }

optsParser :: Parser Config
optsParser = do
  cfgInputFile <- strOption $
    long "input" <>
    metavar "FILE" <>
    help "An input file"
  pure Config{..}

progInfo :: ParserInfo Config
progInfo = info
  (helper <*> optsParser)
  (fullDesc <> header "Run quicksort on a vector endoded in file (produced via Show instance)")

{-# NOINLINE qsortParThread #-}
qsortParThread :: P.Vector Int64 -> IO (P.Vector Int64)
qsortParThread xs = do
  ys <- P.thaw xs
  p  <- Quick.mkParallel . (\n -> n - 1) =<< getNumCapabilities
  Quick.sortInplaceFM p (Quick.Median3or5 @Int64) ys
  Quick.waitParallel p
  P.unsafeFreeze ys

{-# NOINLINE qsortSeq #-}
qsortSeq :: P.Vector Int64 -> IO (P.Vector Int64)
qsortSeq xs = do
  ys <- P.thaw xs
  _  <- Quick.sortInplaceFM Quick.Sequential (Quick.Median3or5 @Int64) ys
  P.unsafeFreeze ys

checkIsSorted :: Ord a => [a] -> Bool
checkIsSorted []     = True
checkIsSorted (x:xs) = go x xs
  where
    go _    []     = True
    go prev (y:ys) = prev <= y && go  y ys

-- _genericSort :: forall v a b. (G.Vector v b, Coercible (v a) (v b), Ord b) => v a -> v a
-- _genericSort = coerce (Quick.sort :: v b -> v b)
-- -- _genericSort xs = ws
-- --   where
-- --     ys :: v b
-- --     ys = coerce xs
-- --
-- --     zs = Quick.sort ys
-- --
-- --     ws :: v a
-- --     ws = coerce zs
-- --
-- --     -- ws = (coerce (Quick.sort (coerce xs :: v b)) :: v a)

main :: IO ()
main = do
  Config{cfgInputFile} <-
    customExecParser (prefs (showHelpOnEmpty <> noBacktrack <> multiSuffix "*")) progInfo

  (!xs :: P.Vector Int64) <-
    if ".cbor" == takeExtension cfgInputFile
    then CBOR.deserialise . BSL.fromStrict <$> BS.readFile cfgInputFile
    else read . C8.unpack <$> BS.readFile cfgInputFile

  -- when False $ do
  --   ys <- P.thaw xs
  --
  --   -- p <- mkParallel =<< getNumCapabilities
  --   -- Quick.sortInplaceFM p (Median3 @Int64) ys
  --   -- waitParallel p
  --
  --   sortPIntParallelMedian3or5IO  ys
  --   when False $ do
  --     evaluate $ rnf ys
  --     hFlush stderr
  --   -- threadDelay 2_000_000
  --   zs <- P.unsafeFreeze ys
  --   putDocLn $ ppVector zs
  --   putDocLn $ "isSorted =" <+> pretty (checkIsSorted (P.toList zs))
  --   putDocLn $ "== L.sort =" <+> pretty (P.toList zs == L.sort (P.toList xs))

  when False $ do
    ys1 <- P.thaw xs
    putStrLn "AveragingMedian"
    Quick.sortInplaceFM Quick.Sequential (AveragingMedian @Int64) ys1
    putDocLn . ppVector =<< P.unsafeFreeze ys1

  -- ys1 <- P.thaw xs
  -- putStrLn "Fast"
  -- QuickFast.sort (Median3or5 @Int64) ys1
  -- -- putDocLn . ppVector =<< P.unsafeFreeze ys1

  -- ys2 <- P.thaw xs
  -- -- putStrLn "Flexible"
  -- p <- mkParallel =<< getNumCapabilities
  -- Quick.sortInplaceFM p (Median3 @Int64) ys2
  -- waitParallel p
  -- -- putDocLn . ppVector =<< P.length P.unsafeFreeze ys2
  -- putDocLn . pretty . P.length =<< P.unsafeFreeze ys2

  -- event' "Sort Seq" $ qsortSeq xs

  -- event' "Sort ParThread" $ qsortParThread xs

  when True $
    concurrently_
      (event' "Sort Seq" $ qsortSeq xs)
      (event' "Sort ParThread" $ qsortParThread xs)

  pure ()

event' :: String -> IO a -> IO a
-- event' _label = id
event' label action = do
  traceEventIO $ "START " ++ label
  start <- getTime Monotonic
  !res <- action
  end <- getTime Monotonic
  traceEventIO $ "STOP "  ++ label
  putStrLn $ printf "%s took %f s" (leftpad 16 label) (fromIntegral (toNanoSecs end - toNanoSecs start) / fromIntegral 1_000_000_000 :: Double)
  pure res

leftpad :: Int -> String -> String
leftpad n str = str ++ replicate (n - length str) ' '
