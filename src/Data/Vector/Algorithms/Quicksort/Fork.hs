----------------------------------------------------------------------------
-- |
-- Module      :  Data.Vector.Algorithms.Quicksort.Fork
-- Copyright   :  (c) Sergey Vinokurov 2023
-- License     :  Apache-2.0 (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

{-# LANGUAGE FunctionalDependencies #-}

module Data.Vector.Algorithms.Quicksort.Fork
  ( HasLength(..)
  , Fork(..)

  , Sequential(..)
  , Parallel
  , ParStrategies(..)
  , mkParallel
  , waitParallel
  , event
  , addPending
  , removePending
  ) where

-- import Debug.Trace qualified

import Control.Concurrent
import Control.Concurrent.Counter (Counter)
import Control.Concurrent.Counter qualified as Counter
import Control.Concurrent.STM
import Control.Monad.ST
import Control.Parallel
import Data.Coerce
import Data.Vector.Generic.Mutable qualified as GM
import Debug.Trace (traceEventIO)
import GHC.Conc.Sync (labelThread)
import GHC.ST (unsafeInterleaveST)
import System.IO.Unsafe

event :: String -> IO a -> IO a
event _label = id
-- event label =
--   bracket_ (traceEventIO $ "START " ++ label)
--            (traceEventIO $ "STOP "  ++ label)

-- asyncLabelled :: String -> IO a -> IO (Async a)
-- asyncLabelled label act = async $ do
--   tid <- myThreadId
--   _ <- labelThread tid label
--   act


class HasLength a where
  getLength :: a -> Int

instance GM.MVector v a => HasLength (v s a) where
  {-# INLINE getLength #-}
  getLength = GM.length

-- | Parallel operation for the quicksort algorithm. Tells how to
-- apply a pair of functions to their respective inputs
class Fork a x m | a -> x where
  startWork :: a -> m x
  endWork   :: a -> x -> m ()
  -- fork
  --   :: (HasLength b, HasLength d)
  --   => a
  --   -> Maybe x
  --   -> (Maybe x -> b -> m c)
  --   -> (Maybe x -> d -> m e)
  --   -> b
  --   -> d
  --   -> m (c, e)
  fork
    :: (HasLength b, HasLength d)
    => a
    -> x
    -> Int
    -> (x -> b -> m ())
    -> (x -> d -> m ())
    -> b
    -> d
    -> m ()

-- | No parallelism
data Sequential = Sequential

instance Monad m => Fork Sequential () m where
  {-# INLINE startWork #-}
  {-# INLINE endWork   #-}
  {-# INLINE fork      #-}
  startWork _ = pure ()
  endWork _ _ = pure ()
  -- fork _ _ f g b d = (,) <$> f Nothing b <*> g Nothing d
  fork _ tok _ f g !b !d = f tok b *> g tok d


-- newtype Semaphore = Semaphore QSem
--
-- newSemaphore :: Int -> IO Semaphore
-- newSemaphore = fmap Semaphore . newQSem
--
-- acquire :: Semaphore -> IO ()
-- acquire (Semaphore x) = waitQSem x
--
-- release :: Semaphore -> IO ()
-- release (Semaphore x) = signalQSem x


newtype Semaphore = Semaphore (TVar Int)

newSemaphore :: Int -> IO Semaphore
newSemaphore = coerce . newTVarIO

acquire :: Semaphore -> IO ()
acquire (Semaphore v) = atomically $ do
  !n <- readTVar v
  if n > 0
  then writeTVar v $! n - 1
  else retry

release :: Semaphore -> IO ()
release (Semaphore v) = atomically $ do
  modifyTVar' v (+ 1)


-- | At most N concurrent jobs at any given time.
-- newtype Parallel = Parallel Counter
-- newtype Parallel = Parallel (TVar Int)
data Parallel = Parallel !Semaphore !(TVar Int) !Counter

mkParallel :: Int -> IO Parallel
mkParallel hint =
  Parallel <$> newSemaphore hint <*> newTVarIO 0 <*> Counter.new 0
  -- coerce (Counter.new hint)
  -- coerce (Counter.new 0)
  -- coerce (newTVarIO (0 :: Int))

addPending :: Parallel -> IO ()
addPending (Parallel _ pending _) =
  atomically $ modifyTVar' pending (+ 1)

removePending :: Parallel -> IO ()
removePending (Parallel _ pending _) =
  atomically $ modifyTVar' pending $ \x -> x - 1

waitParallel :: Parallel -> Int -> IO ()
waitParallel (Parallel _ pending _) _ = atomically $ do
  -- k <- readTVar p
  -- m <- readTVar pending
  -- if k == n && m == 0
  -- then pure ()
  -- else retry
  m <- readTVar pending
  if m == 0
  then pure ()
  else retry

leftpad :: Int -> String -> String
leftpad n xs = replicate (n - (length xs)) '0' ++ xs

instance Fork Parallel (Maybe Int) IO where
  {-# INLINE startWork #-}
  {-# INLINE endWork   #-}
  {-# INLINE fork      #-}
  startWork (Parallel s _ _cnt) = do
    -- tid <- myThreadId
    -- putStrLn $ "Start work, tid = " ++ show tid
    acquire s
    -- n <- Counter.add cnt 1
    -- traceEventIO $ "START thread " ++ leftpad 6 (show n)
    -- putStrLn $ "START thread " ++ leftpad 6 (show n)
    pure (Just 0)
  endWork Parallel{}         Nothing = pure ()
  endWork p@(Parallel s _ _) Just{}  = do
    release s
    removePending p
    -- traceEventIO $ "STOP thread " ++ leftpad 6 (show n)
    -- putStrLn $ "END thread " ++ leftpad 6 (show n)

  -- fork
  --   :: forall b d c e. (HasLength b, HasLength d)
  --   => Parallel
  --   -> Maybe Int
  --   -> (Maybe Int -> b -> IO c)
  --   -> (Maybe Int -> d -> IO e)
  --   -> b
  --   -> d
  --   -> IO (c, e)
  -- fork _ Nothing f g !b !d
  --   = (,) <$> f Nothing b <*> g Nothing d
  -- fork !p tok@(Just releaseToken) f g !b !d
  --   -- | bigB && bigD
  --   -- = withAsync (startWork p >>= \token -> f (Just token) b) $ \jobB -> do
  --   --     withAsync (startWork p >>= \token -> g (Just token) d) $ \jobD -> do
  --   --       endWork p releaseToken
  --   --       res <- event "waiting on results" $ (,) <$> wait jobB <*> wait jobD
  --   --       pure res
  --   | bigB
  --   = withAsync (startWork p >>= \token -> f (Just token) b) $ \jobB -> do
  --       d' <- g tok d
  --       b' <- event ("waiting on results " ++ leftpad 6 (show releaseToken)) $ wait jobB
  --       pure (b', d')
  --   | bigD
  --   = withAsync (startWork p >>= \token -> g (Just token) d) $ \jobD -> do
  --       b' <- f tok b
  --       d' <- event ("waiting on results " ++ leftpad 6 (show releaseToken)) $ wait jobD
  --       pure (b', d')
  --   | otherwise
  --   = (,) <$> f Nothing b <*> g Nothing d <* endWork p releaseToken
  --   where
  --     bigB = getLength b > 1000
  --     bigD = getLength d > 1000

  fork
    :: forall b d. (HasLength b, HasLength d)
    => Parallel
    -> Maybe Int
    -> Int
    -> (Maybe Int -> b -> IO ())
    -> (Maybe Int -> d -> IO ())
    -> b
    -> d
    -> IO ()
  fork _ Nothing _ f g !b !d
    = f Nothing b *> g Nothing d
  fork !p tok@(Just _tok') !_depth f g !b !d
    -- | depth < 4 && Debug.Trace.trace ("depth = " ++ show depth ++ ", bLen = " ++ show bLen ++ ", dLen = " ++ show dLen) False = undefined
    -- | bigB && bigD
    | mn > 1000 && mn * 4 >= mx
    -- | mn > 1000 && mn * 4 >= mx
    = do
      addPending p
      _ <- forkIO $ startWork p >>= \token -> f token b
      g tok d
    -- | bigD
    -- = do
    --   addPending p
    --   _ <- forkIO $ startWork p >>= \token -> g (Just token) d
    --   f tok b
    -- | otherwise
    -- = f Nothing b *> g Nothing d *> endWork p tok'
    | bLen > dLen
    = g Nothing d *> f tok b
    | otherwise
    = f Nothing b *> g tok d
    where
      bLen, dLen :: Int
      !bLen = getLength b
      !dLen = getLength d
      -- bigB, bigD :: Bool
      -- !bigB = bLen > 1000
      -- !bigD = dLen > 1000

      !mn = min bLen dLen
      !mx = max bLen dLen


  -- fork (Parallel sync) f g b d
    -- | getLength b > 1000
    -- = do
    --   old <- Counter.sub sync 1
    --   if old <= 0
    --   then do
    --     _ <- Counter.add sync 1
    --     (,) <$> f b <*> g d
    --   else
    --     withAsync (f b <* Counter.add sync 1) $ \job -> do
    --       d' <- g d
    --       -- Let others run for a while since current thread is blocked
    --       -- waiting.
    --       _  <- Counter.add sync 1
    --       b' <- wait job
    --       -- Restore the balance
    --       _  <- Counter.sub sync 1
    --       pure (b', d')
    -- | otherwise
    -- = (,) <$> f b <*> g d

-- instance Fork Parallel IO where
--   {-# INLINE fork #-}
--   fork (Parallel sync) f g b d = do
--     -- atomically $ modifyTVar' sync (\x -> x + 1)
--     _old <- Counter.add sync 1
--     (,) <$> f b <*> g d
--     -- | getLength b > 1000
--     -- = do
--     --   -- _old <- Counter.sub sync 1
--     --   atomically $ modifyTVar' sync (\x -> x + 1)
--     --   (\y x -> (x, y)) <$> g d <*> f b
--     --   -- old <- Counter.sub sync 1
--     --   -- if old <= 0
--     --   -- then do
--     --   --   _ <- Counter.add sync 1
--     --   --   (,) <$> f b <*> g d
--     --   -- else
--     --   --   error "never happens"
--     --     -- withAsync (f b) $ \job -> do
--     --     --   d' <- g d
--     --     --   -- Let others run for a while since current thread is blocked
--     --     --   -- waiting.
--     --     --   -- _  <- Counter.add sync 1
--     --     --   b' <- wait job
--     --     --   -- Restore the balance
--     --     --   -- _  <- Counter.sub sync 1
--     --     --   pure (b', d')
--     -- | otherwise
--     -- = (,) <$> f b <*> g d


data ParStrategies = ParStrategies

instance Fork ParStrategies () IO where
  {-# INLINE startWork #-}
  {-# INLINE endWork   #-}
  {-# INLINE fork      #-}
  startWork _ = pure ()
  endWork _ _ = pure ()

  fork
    :: forall b d. (HasLength b, HasLength d)
    => ParStrategies
    -> ()
    -> Int
    -> (() -> b -> IO ())
    -> (() -> d -> IO ())
    -> b
    -> d
    -> IO ()

  -- fork _ _ _ f g !b !d
  --   = do
  --     b' <- unsafeInterleaveIO (f () b)
  --     b' `par` g () d

  -- fork _ Nothing _ f g !b !d
  --   = f Nothing b *> g Nothing d

  fork _ _ _depth f g !b !d
    -- = do
    --   let d' = unsafePerformIO $ g () d
    --   let b' = unsafePerformIO $ f () b
    --   d' `par` b' `par` pure (d' `pseq` b' `pseq` ())

    -- | Debug.Trace.trace ("depth = " ++ show _depth ++ ", bLen = " ++ show bLen ++ ", dLen = " ++ show dLen) False = undefined

    -- | bigB && bigD

    -- | depth < 8
    -- = do
    --   let d' = unsafePerformIO $ g () d
    --   let b' = unsafePerformIO $ f () b
    --   d' `par` b' `par` pure (d' `pseq` b' `pseq` ())

    | mn > 100 && mn * 10 >= mx
    -- | mn > 1000 && mn * 4 >= mx
    = do
      let b' = unsafePerformIO $ f () b
      d' <- b' `par` g () d
      pure (b' `seq` (d' `seq` ()))

    | otherwise
    = do
      b' <- f () b
      d' <- g () d
      pure (b' `seq` (d' `seq` ()))

    -- | bLen > dLen
    -- = do
    --   let d' = unsafePerformIO $ g () d
    --   () <- d' `par` f () b
    --   pure (d' `pseq` ())
    -- | otherwise
    -- = do
    --   let b' = unsafePerformIO $ f () b
    --   () <- b' `par`  g () d
    --   pure (b' `pseq` ())

    where
      bLen, dLen :: Int
      !bLen = getLength b
      !dLen = getLength d
      -- bigB, bigD :: Bool
      -- !bigB = bLen > 1000
      -- !bigD = dLen > 1000

      !mn = min bLen dLen
      !mx = max bLen dLen

instance Fork ParStrategies () (ST s) where
  {-# INLINE startWork #-}
  {-# INLINE endWork   #-}
  {-# INLINE fork      #-}
  startWork _ = pure ()
  endWork _ _ = pure ()

  fork
    :: forall b d. (HasLength b, HasLength d)
    => ParStrategies
    -> ()
    -> Int
    -> (() -> b -> ST s ())
    -> (() -> d -> ST s ())
    -> b
    -> d
    -> ST s ()

  -- fork _ _ _ f g !b !d
  --   = do
  --     b' <- unsafeInterleaveIO (f () b)
  --     b' `par` g () d

  -- fork _ Nothing _ f g !b !d
  --   = f Nothing b *> g Nothing d

  fork _ _ _depth f g !b !d
    -- = do
    --   let d' = unsafePerformIO $ g () d
    --   let b' = unsafePerformIO $ f () b
    --   d' `par` b' `par` pure (d' `pseq` b' `pseq` ())

    -- | Debug.Trace.trace ("depth = " ++ show _depth ++ ", bLen = " ++ show bLen ++ ", dLen = " ++ show dLen) False = undefined

    -- | bigB && bigD

    -- | depth < 8
    -- = do
    --   let d' = unsafePerformIO $ g () d
    --   let b' = unsafePerformIO $ f () b
    --   d' `par` b' `par` pure (d' `pseq` b' `pseq` ())

    | mn > 100 && mn * 10 >= mx
    -- | mn > 1000 && mn * 4 >= mx
    = do
      b' <- unsafeInterleaveST $ f () b
      d' <- b' `par` g () d
      pure (b' `seq` (d' `seq` ()))

    | otherwise
    = do
      b' <- f () b
      d' <- g () d
      pure (b' `seq` (d' `seq` ()))

    -- | bLen > dLen
    -- = do
    --   let d' = unsafePerformIO $ g () d
    --   () <- d' `par` f () b
    --   pure (d' `pseq` ())
    -- | otherwise
    -- = do
    --   let b' = unsafePerformIO $ f () b
    --   () <- b' `par`  g () d
    --   pure (b' `pseq` ())

    where
      bLen, dLen :: Int
      !bLen = getLength b
      !dLen = getLength d
      -- bigB, bigD :: Bool
      -- !bigB = bLen > 1000
      -- !bigD = dLen > 1000

      !mn = min bLen dLen
      !mx = max bLen dLen
