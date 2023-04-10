-- |
-- Module:     Data.Vector.Algorithms.Quicksort.Fork2
-- Copyright:  (c) Sergey Vinokurov 2023
-- License:    Apache-2.0 (see LICENSE)
-- Maintainer: serg.foo@gmail.com
--
-- This module defines how quicksort is parallelised. The 'Fork2' class
-- is the main source of parallelisation strategies.

{-# LANGUAGE FunctionalDependencies #-}

module Data.Vector.Algorithms.Quicksort.Fork2
  (

  -- * Main interface
    Fork2(..)

  , Sequential(..)
  , Parallel
  , ParStrategies(..)
  , mkParallel
  , waitParallel

  -- * Helpers
  , HasLength
  , getLength
  ) where

import GHC.Conc (par, pseq)

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad.ST
import Data.Bits
import Data.Vector.Generic.Mutable qualified as GM
import GHC.ST (unsafeInterleaveST)
import System.IO.Unsafe

-- | Parallelization strategy for the quicksort algorithm with
-- single-pivot partitioning. Specifies how to apply a pair of functions
-- to their respective inputs (which will be recursive quicksort calls).
--
-- NB the name @Fork2@ suggests that two threads will be only forked.
--
-- Parameter meaning;
-- - @a@ - the parallelisation we're defining instance for
-- - @x@ - type of tokens that strategy can pass around to track recursive calls
-- - @m@ - monad the strategy operates in. Some strategies only make
--   sense in a particular monad, e.g. parellelisation via 'forkIO'
class Fork2 a x m | a -> x where
  -- | Will get called by quicksort when sorting starts.
  startWork :: a -> m x
  -- | Will get called by quicksort when it finishes sorting its array.
  endWork   :: a -> x -> m ()
  fork2
    :: (HasLength b, HasLength d)
    => a                -- ^ Parallelisation algorithm that can carry
                        -- extra info, e.g. for synchronization
    -> x                -- ^ "Token" for current execution thread,
                        -- will be passed to 'endWork' when done
    -> Int              -- ^ Recursion depth
    -> (x -> b -> m ()) -- ^ One recursive quicksort call
    -> (x -> d -> m ()) -- ^ The other recursive quicksort call
    -> b                -- ^ One of the subarrays after partitioning to be sorted
    -> d                -- ^ The other subarray to be sorted
    -> m ()

-- | Trivial parallelisation strategy that executes everything
-- sequentially in current thread. Good default overall.
data Sequential = Sequential

instance Monad m => Fork2 Sequential () m where
  {-# INLINE startWork #-}
  {-# INLINE endWork   #-}
  {-# INLINE fork2     #-}
  startWork _ = pure ()
  endWork _ _ = pure ()
  fork2 _ tok _ f g !b !d = f tok b *> g tok d

-- | At most N concurrent jobs will be spawned to evaluate recursive calls after quicksort
-- partitioning.
--
-- Warning: currently not as fast as sparks-based 'ParStrategies'
-- strategy, take care to benchmark before using.
data Parallel = Parallel !Int !(TVar Int)

-- | Make parallelisation strategy with at most @N@ threads.
mkParallel :: Int -> IO Parallel
mkParallel jobs =
  Parallel jobs <$> newTVarIO 0

addPending :: Parallel -> IO ()
addPending (Parallel _ pending) =
  atomically $ modifyTVar' pending (+ 1)

removePending :: Parallel -> IO ()
removePending (Parallel _ pending) =
  atomically $ modifyTVar' pending $ \x -> x - 1

-- | Wait until all threads related to a particular 'Parallel' instance finish.
waitParallel :: Parallel -> IO ()
waitParallel (Parallel _ pending) = atomically $ do
  m <- readTVar pending
  if m == 0
  then pure ()
  else retry

instance Fork2 Parallel (Bool, Bool) IO where
  {-# INLINE startWork #-}
  {-# INLINE endWork   #-}
  {-# INLINE fork2     #-}
  startWork !p = do
    addPending p
    pure (False, True)

  endWork p (_, shouldDecrement)
    | shouldDecrement
    = removePending p
    | otherwise
    = pure ()

  fork2
    :: forall b d. (HasLength b, HasLength d)
    => Parallel
    -> (Bool, Bool)
    -> Int
    -> ((Bool, Bool) -> b -> IO ())
    -> ((Bool, Bool) -> d -> IO ())
    -> b
    -> d
    -> IO ()
  fork2 !p@(Parallel jobs _) tok@(!isSeq, shouldDecrement) !depth f g !b !d
    | isSeq
    = f (True, False) b *> g tok d
    | 2 `unsafeShiftL` depth < jobs && mn > 100 && mn * 10 > mx
    = do
      addPending p
      _ <- forkIO $ f (False, True) b
      g tok d
    | bLen > dLen
    = f (False, False) b *> g (True, shouldDecrement) d
    | otherwise
    = g (False, False) d *> f (True, shouldDecrement) b
    where
      bLen, dLen :: Int
      !bLen = getLength b
      !dLen = getLength d

      (!mn, !mx) = if bLen < dLen then (bLen, dLen) else (dLen, bLen)

-- | Parallelise with sparks. After partitioning if sides are
-- sufficiently big then spark will be created to evaluate one of the
-- parts while another will continue to be evaluated in current
-- execution thread.
--
-- This strategy works in both 'IO' and 'ST' monads (see docs for
-- relevant instance for some discussion on how that works).
--
-- Sparks will seamlessly use all available RTS capabilities
-- (configured with @+RTS -N@ flag) and according to benchmarks in
-- this package have pretty low synchronization overhead as opposed to
-- thread-based parallelisation that 'Parallel' offers. These benefits
-- allow sparks to work on much smaller chunks and exercise more
-- parallelism.
data ParStrategies = ParStrategies

instance Fork2 ParStrategies () IO where
  {-# INLINE startWork #-}
  {-# INLINE endWork   #-}
  {-# INLINE fork2     #-}
  startWork _ = pure ()
  endWork _ _ = pure ()

  fork2
    :: forall b d. (HasLength b, HasLength d)
    => ParStrategies
    -> ()
    -> Int
    -> (() -> b -> IO ())
    -> (() -> d -> IO ())
    -> b
    -> d
    -> IO ()
  fork2 _ _ _ f g !b !d
    | mn > 100 && mn * 10 > mx
    = do
      let b' = unsafePerformIO $ f () b
      d' <- b' `par` g () d
      pure (b' `pseq` (d' `pseq` ()))
    | otherwise
    = do
      b' <- f () b
      d' <- g () d
      pure (b' `pseq` (d' `pseq` ()))
    where
      bLen, dLen :: Int
      !bLen = getLength b
      !dLen = getLength d

      !mn = min bLen dLen
      !mx = max bLen dLen

-- | This instance is a bit surprising - ST monad, after all, doesn’t
-- have concurrency and threads everywhere its @s@ parameter to
-- signal, among other things, that it’s single execution thread.
--
-- Still, quicksort in this package hopefully doesn’t do anything
-- funny that may break under parallelism. Use of this instance for
-- other purposes has at least the same caveats as use of
-- 'unsafeInterleaveST' (i.e. not recommended, especially considering
-- that the instance may change).
instance Fork2 ParStrategies () (ST s) where
  {-# INLINE startWork #-}
  {-# INLINE endWork   #-}
  {-# INLINE fork2     #-}
  startWork _ = pure ()
  endWork _ _ = pure ()

  fork2
    :: forall b d. (HasLength b, HasLength d)
    => ParStrategies
    -> ()
    -> Int
    -> (() -> b -> ST s ())
    -> (() -> d -> ST s ())
    -> b
    -> d
    -> ST s ()
  fork2 _ _ _ f g !b !d
    | mn > 100 && mn * 10 > mx
    = do
      b' <- unsafeInterleaveST $ f () b
      d' <- b' `par` g () d
      pure (b' `pseq` (d' `pseq` ()))
    | otherwise
    = do
      b' <- f () b
      d' <- g () d
      pure (b' `pseq` (d' `pseq` ()))
    where
      bLen, dLen :: Int
      !bLen = getLength b
      !dLen = getLength d

      !mn = min bLen dLen
      !mx = max bLen dLen

-- | Helper that can be used to estimatae sizes of subproblems.
--
-- For inscance, too small array will not benefit from sorting it in
-- parallel because parallelisation overhead will likely trump any
-- time savings.
class HasLength a where
  -- | Length of item
  getLength :: a -> Int

instance GM.MVector v a => HasLength (v s a) where
  {-# INLINE getLength #-}
  getLength = GM.length

