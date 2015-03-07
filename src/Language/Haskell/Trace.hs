{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

-- | Tracing API.
--
-- This API anticipates lots of calls to `trace` in a small amount of
-- time. So it merely writes to a buffer which is flushed every so
-- often to avoid abusive disk writes.
--
-- Writes to the file designated by 'logPath'.

module Language.Haskell.Trace
  (trace
  ,logPath)
  where

import           Control.Concurrent
import           Control.Exception
import           Control.Monad
import           Control.Monad.Fix
import           Data.IORef
import           Data.Monoid
import qualified Data.Text.Lazy as LT
import           Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy.Builder as LT
import qualified Data.Text.Lazy.IO as LT
import           System.IO
import           System.IO.Unsafe

--------------------------------------------------------------------------------
-- Constants

-- | Log file path. Outputs in the current directory for simplicity.
logPath :: FilePath
logPath = "haskell-trace.log"

--------------------------------------------------------------------------------
-- Unsafe module-global bindings

-- | Buffer for all the output.
buffer :: IORef Builder
buffer = unsafePerformIO (newIORef mempty)
{-# NOINLINE buffer #-}

-- | A var to know whether the buffer thread is running or not.
threadVar :: MVar ()
threadVar = unsafePerformIO newEmptyMVar
{-# NOINLINE threadVar #-}

-- | Wrap up an expression with a tracing of its original source
-- location. Writes to the output buffer and then automatically
-- launches the buffer thread if it's not already running.
--
trace :: FilePath -> (Int,Int,Int,Int) -> ()
trace moduleFile (sl,sc,el,ec) =
  unsafePerformIO
    (do atomicModifyIORef' buffer
                           (\b -> (b <> msg,()))
        grabbed <- tryPutMVar threadVar ()
        when grabbed
             (void (forkIO bufferThread)))
  where msg =
          build moduleFile <> ":(" <>
          build (show sl) <>
          "," <>
          build (show sc) <>
          ")-(" <>
          build (show el) <>
          "," <>
          build (show (ec - 1)) <>
          ")\n"
          where build = LT.fromLazyText . LT.pack
{-# NOINLINE trace #-}

--------------------------------------------------------------------------------
-- Background activities

-- | A thread launched on the current buffer. It opens the log file in
-- append mode and atomically reads from the buffer IORef. If the
-- buffer is empty, it stops and closes the file, restoring the thread
-- var. Otherwise it loops until the buffer is empty.
--
-- The reason for this conservative way of ending is to handle the
-- case of running inside GHCi where one would have to deal with
-- object lifetime; it's simpler to bail out early and not worry about
-- this.
--
bufferThread :: IO ()
bufferThread =
  do h <- openFile logPath AppendMode
     finally (fix (\loop ->
                     do builder <-
                          atomicModifyIORef'
                            buffer
                            (\b -> (mempty,b))
                        unless (builder == mempty)
                               (do LT.hPutStr h
                                              (LT.toLazyText builder)
                                   threadDelay (1000 * 500)
                                   loop)))
             (do hClose h
                 takeMVar threadVar)
