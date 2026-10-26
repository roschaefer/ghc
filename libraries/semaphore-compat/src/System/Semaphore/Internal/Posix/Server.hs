{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | The Unix domain socket server backing the POSIX semaphore implementation.
--
-- The server manages a shared token pool and accepts multiple client
-- connections, each served on its own thread.
--
-- Protocol (SOCK_STREAM, one byte per command):
--
--   "-"  Wait (blocking acquire).  Decrements semaphore; replies ".".
--   "?"  Try-wait.  Decrements if positive and replies "."; otherwise replies "!".
--   "+"  Release.  Increments pool; replies ".".  Rejected with "!" if
--        myCount <= 0 (client has not acquired any tokens on this connection).
--
-- Unrecognised bytes are rejected with 'RspFail'.
--
-- Per-connection token tracking: the server counts tokens held by each
-- connection (myCount).  On disconnect (EOF / ResourceVanished) held
-- tokens are returned to the pool, so a crashing client cannot leak
-- tokens.
--
-- Connections are tied to a token, so each token (obtained by
-- @waitOnSemaphore@) has its own connection.
module System.Semaphore.Internal.Posix.Server
  ( serverLoop
  , pattern CmdWait, pattern CmdTryWait, pattern CmdRelease
  , pattern RspOk, pattern RspFail
  ) where

-- base
import Control.Concurrent
  ( ThreadId, forkIOWithUnmask, killThread )
import Control.Concurrent.MVar
  ( MVar, newEmptyMVar, newMVar, putMVar
  , readMVar, takeMVar, tryTakeMVar )
import Control.Exception
  ( IOException, SomeException
  , catch, finally, mask, mask_
  , onException, throw, try, uninterruptibleMask_
  )
import Control.Monad
  ( forM_, forever, void, when )
import Data.Word ( Word8 )
import Foreign.C.Error ( Errno(Errno), eCONNABORTED )
import GHC.IO.Exception ( ioe_errno )
import System.IO.Error ( isFullError )

-- stm
import Control.Concurrent.STM
  ( TVar, atomically, newTVarIO, readTVar, readTVarIO
  , modifyTVar', writeTVar, retry )

-- unix
import System.Posix.IO ( closeFd )
import System.Posix.Types ( Fd )

import System.Semaphore.Internal.DomainSocket
  ( pollAcceptSocket, AcceptResult(..)
  , fdReadByte, fdWriteByte
  , fdShutdown )

---------------------------------------
-- Protocol byte constants

pattern CmdWait, CmdTryWait, CmdRelease :: Word8
pattern CmdWait    = 0x2D -- '-'
pattern CmdTryWait = 0x3F -- '?'
pattern CmdRelease = 0x2B -- '+'

pattern RspOk, RspFail :: Word8
pattern RspOk   = 0x2E -- '.'
pattern RspFail = 0x21 -- '!'

-- | Children of the server, to be cleaned up
-- childFdLock is full when the 'Fd' is still known to be valid,
-- and subject to being shutdown or closed
data Child = Child
  { childThread :: !(MVar ThreadId)
  , childFdLock :: !(MVar Fd)
  }

-- | Run the server accept loop on a listening socket until cancellation is
-- signalled via the cancel pipe.  All accepted connections are served on
-- child threads, which are cleaned up before this returns.
serverLoop :: TVar Int -- ^ shared token pool
           -> Fd        -- ^ listening socket
           -> Fd        -- ^ read end of the cancel pipe
           -> IO ()
serverLoop pool listenFd cancelFd = do
    children <- newTVarIO ([] :: [Child])
    loop children `finally` killChildren children
  where
    loop children = do
      continueLoop <- mask_ $ do
        r <- acceptWithRetry
        case r of
          AcceptCancelled     -> return False
          AcceptedFd clientFd -> do
            forkServeChild children clientFd
            return True
      when continueLoop $ loop children

    -- pollAcceptSocket only returns if the accept succeeded, or cancellation
    -- was signalled via the cancel pipe.
    acceptWithRetry :: IO AcceptResult
    acceptWithRetry = pollAcceptSocket listenFd cancelFd `catch` handleIOError

    -- Retry accept on transient errors.
    handleIOError :: IOException -> IO AcceptResult
    handleIOError e
      -- 'isFullError' catches ResourceExhausted (EMFILE/ENFILE/ENOBUFS/ENOMEM and more that accept doesn't produce but are harmless to retry).
      | isFullError e                             = acceptWithRetry
      -- ECONNABORTED is also transient but categorised as OtherError, we additionaly match that.
      | Just err <- ioe_errno e
      , Errno err == eCONNABORTED                 = acceptWithRetry
      -- EINTR is absorbed in hs_poll_accept.
      -- everything else (EBADF, EINVAL, ENOTSOCK, ...) is rethrown.
      | otherwise                                 = throw e

    forkServeChild children clientFd = do
      fdLock <- newMVar clientFd
      tidVar <- newEmptyMVar
      let child = Child tidVar fdLock
      atomically $ modifyTVar' children (child :)
      childTid <- forkIOWithUnmask $ \unmask ->
        serve unmask pool children clientFd child
      putMVar tidVar childTid

    -- Interrupt all children blocked on a read from the FD, then kill them.
    killChildren :: TVar [Child] -> IO ()
    killChildren children = do
      kids <- readTVarIO children
      forM_ kids $ \child -> do
        -- If the child is in a read(), interrupt it
        -- by calling 'fdShutdown'.
        mb <- tryTakeMVar (childFdLock child)
        case mb of
          Just cfd -> do
            void $ try @IOException $ fdShutdown cfd
            putMVar (childFdLock child) cfd
          Nothing  -> return ()  -- serve thread already closing
      -- No children blocked on read: terminate them all.
      -- childThread was filled by the parent before mask exit; readMVar of
      -- a definitely-full MVar is non-interruptible.
      forM_ kids $ \child -> do
        tid <- readMVar (childThread child)
        killThread tid

-- | Per-connection server loop.
serve :: (forall a. IO a -> IO a)
      -> TVar Int -> TVar [Child] -> Fd -> Child
      -> IO ()
serve restore pool children fd (Child _ fdLock) = do
    myCount <- newTVarIO (0 :: Int)
    let loop = forever $ mask $ \restoreInner -> do
            -- fdReadByte is a safe-FFI read(2), interrupted by
            -- fdShutdown from killChildren, not by throwTo.
            msg <- fdReadByte fd
            case msg of
              CmdWait -> do
                -- Block until a token is available.
                -- restoreInner keeps retry interruptible under mask.
                restoreInner $ atomically $ do
                    n <- readTVar pool
                    when (n <= 0) retry
                    writeTVar pool (n - 1)
                    modifyTVar' myCount (+ 1)
                fdWriteByte fd RspOk

              CmdRelease -> do
                ok <- atomically $ do
                    mc <- readTVar myCount
                    if mc > 0
                      then do
                        modifyTVar' pool (+ 1)
                        modifyTVar' myCount (subtract 1)
                        return True
                      else return False
                fdWriteByte fd (if ok then RspOk else RspFail)

              CmdTryWait -> do
                acquired <- atomically $ do
                    n <- readTVar pool
                    if n > 0
                      then do
                        writeTVar pool (n - 1)
                        modifyTVar' myCount (+ 1)
                        return True
                      else return False
                if acquired
                  then fdWriteByte fd RspOk
                  else fdWriteByte fd RspFail

              -- Unknown command: reply so the client doesn't hang in read().
              _ -> fdWriteByte fd RspFail

        cleanup = do
          -- Return tokens to the pool and remove from children list.
          atomically $ do
            n <- readTVar myCount
            when (n > 0) $ modifyTVar' pool (+ n)
            modifyTVar' children (filter (\c -> childFdLock c /= fdLock))
          -- Take fd ownership and close.
          -- prevents killChildren from double closing fd
          void $ takeMVar fdLock
          void $ try @IOException $ closeFd fd

    -- restore so thread can be killed in between loop iterations
    -- Catch IOException (EOF/disconnect) silently.
    (restore loop `catch` \(_ :: IOException) -> return ())
      `finally` cleanup
