{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | POSIX backend: semaphores implemented on top of Unix domain sockets.
module System.Semaphore.Internal.Posix
  ( ClientSemaphore(..), ServerSemaphore(..)
  , SemaphoreToken(..)
  , waitOnSemaphore, tryWaitOnSemaphore
  , releaseSemaphoreToken
  , destroyClientSemaphore, destroyServerSemaphore
  , getSemaphoreValue

  , create_sem, open_sem_raw
  , get_time_seed
  ) where

-- base
import Control.Concurrent
  ( ThreadId, forkIOWithUnmask, killThread )
import Control.Concurrent.MVar
  ( MVar, mkWeakMVar, newEmptyMVar, newMVar, putMVar
  , takeMVar, tryTakeMVar )
import Control.Exception
  ( IOException, SomeException
  , finally, mask_, onException, throw, try, uninterruptibleMask_
  )
import Control.Monad ( void )
import Data.Bits ( xor )
import GHC.Clock ( getMonotonicTimeNSec )
import GHC.Stack ( HasCallStack )

-- directory
import System.Directory ( doesPathExist )

-- stm
import Control.Concurrent.STM
  ( TVar, newTVarIO, readTVarIO )

-- unix
import System.Posix.IO ( closeFd, createPipe, setFdOption, FdOption(CloseOnExec) )
import System.Posix.Files ( removeLink )
import System.Posix.Types ( Fd )
import System.Posix.Process ( getProcessID )

-- semaphore-compat
import System.Semaphore.Internal.Version
import System.Semaphore.Internal.DomainSocket
  ( connectDomainSocket, listenDomainSocket
  , fdReadByte, fdWriteByte
  , fdShutdown )
import System.Semaphore.Internal.Posix.Server
  ( serverLoop
  , pattern CmdWait, pattern CmdTryWait, pattern CmdRelease
  , pattern RspOk, pattern RspFail
  )

--------------------------------------------------------------------------------

-- A client is just an identity (name + socket path); each operation that needs
-- a connection opens one internally.
data ClientSemaphore =
  ClientSemaphore
    { clientSemaphoreName :: !SemaphoreName
    , semSocketPath :: !FilePath
    }

-- The fd is held in the 'MVar' so 'releaseSemaphoreToken' takes ownership
-- atomically: a second (erroneous) release is a safe no-op.
newtype SemaphoreToken = SemaphoreToken
  { tokenFdLock :: MVar Fd
    -- NB: the 'Fd' is held in an 'MVar' to allow 'releaseSemaphoreToken'
    -- to takes ownership of the 'Fd' atomically. This ensures that a
    -- second (erroneous) release is a safe no-op (avoiding Fd double-close bugs).
  }

data ServerSemaphore = ServerSemaphore
  { serverClientSemaphore :: !ClientSemaphore
  , serverThreadId :: !ThreadId
  , serverPool     :: !(TVar Int)
  , serverState    :: !(MVar ServerState)
      -- NB: the MVar is emptied when resources/fds are freed to prevent double close
  }

data ServerState = ServerState
  { serverListenFd :: !Fd
  , serverCancelFd :: !Fd
    -- Write end of the cancel pipe.  Writing a byte signals the
    -- server loop to exit its poll-accept.
  }

create_sem :: SemaphoreName -> Int -> IO (Either SemaphoreError ServerSemaphore)
create_sem sem_nm init_toks = do
  mb_res <- try @IOException $ mask_ $ do
    socketPath <- getSemaphoreSocketPath sem_nm
    listenFd <- listenDomainSocket socketPath
    -- ^^ creates the socket file, unlink it too on failure.
    let cleanupListen = do
          void $ try @IOException $ closeFd listenFd
          void $ try @IOException $ removeLink socketPath
    flip onException cleanupListen $ do
      pool <- newTVarIO init_toks
      (cancelRd, cancelWr) <- createPipe
      setFdOption cancelRd CloseOnExec True
      setFdOption cancelWr CloseOnExec True
      tid <- forkIOWithUnmask $ \unmask ->
               unmask (serverLoop pool listenFd cancelRd)
                 `finally` closeFd cancelRd
      stateVar <- newMVar ServerState
        { serverListenFd = listenFd
        , serverCancelFd = cancelWr
        }
      return ServerSemaphore
        { serverClientSemaphore = ClientSemaphore { clientSemaphoreName = sem_nm
                                            , semSocketPath = socketPath }
        , serverThreadId  = tid
        , serverPool      = pool
        , serverState     = stateVar
        }
  return $ case mb_res of
    Left  err -> Left $ SemaphoreOtherError err
    Right sem -> Right sem

open_sem_raw :: SemaphoreName -> IO (Either SemaphoreError ClientSemaphore)
open_sem_raw nm = do
  mb_res <- try @IOException $ do
    socketPath <- getSemaphoreSocketPath nm
    exists <- doesPathExist socketPath
    return (socketPath, exists)
  return $ case mb_res of
    Left  err             -> Left $ SemaphoreOtherError err
    Right (_, False)      -> Left $ SemaphoreDoesNotExist (semaphoreIdentifier nm)
    Right (socketPath, _) -> Right $
      ClientSemaphore
        { clientSemaphoreName = nm
        , semSocketPath = socketPath
        }

waitOnSemaphore :: HasCallStack => ClientSemaphore -> IO SemaphoreToken
waitOnSemaphore sem = do
  resultVar <- newEmptyMVar
  -- Mask exceptions until we get to the interruptible takeMVar
  mask_ $ do
    fd <- connectDomainSocket (semSocketPath sem)
    -- The read() runs in a forked thread
    workerTid <- forkIOWithUnmask $ \_ -> do
      res <- try @SomeException $ do
        fdWriteByte fd CmdWait
        fdReadByte fd
      putMVar resultVar res
    -- uninterruptibleMask_: killThread is interruptible, and a
    -- second async between killThread and closeFd would leak fd.
    let cleanup = uninterruptibleMask_ $ do
          -- shutdown(SHUT_RDWR) causes the worker's read() to return EOF immediately
          void $ try @IOException $ fdShutdown fd
          -- Wait for the worker to exit before closing the fd.
          -- this prevents double close bugs
          killThread workerTid
          -- Finally close the fd
          void $ try @IOException $ closeFd fd
    -- We achieve interruptiblity by relying on the interruptiblity of takeMVar
    -- The worker thread is blocked on read(), fdShutdown in cleanup interrupts
    -- it if we are interrupt by an async exception here.
    res <- takeMVar resultVar `onException` cleanup
    case res of
      Right resp
        | resp == RspOk -> mkToken fd
        | otherwise     -> do void $ try @IOException $ closeFd fd
                              fail $ "semaphore-compat: unexpected response in waitOnSemaphore: " ++ show resp
      Left e            -> do void $ try @IOException $ closeFd fd
                              throw e

tryWaitOnSemaphore :: HasCallStack => ClientSemaphore -> IO (Maybe SemaphoreToken)
tryWaitOnSemaphore sem =
  mask_ $ do
    fd <- connectDomainSocket (semSocketPath sem)
    resp <- flip onException (closeFd fd) $ do
      fdWriteByte fd CmdTryWait
      fdReadByte fd
    case resp of
      RspOk -> Just <$> mkToken fd
      _     -> do void $ try @IOException $ closeFd fd
                  return Nothing

mkToken :: Fd -> IO SemaphoreToken
mkToken fd = do
  fdVar <- newMVar fd
  _ <- mkWeakMVar fdVar $ do
    mb <- tryTakeMVar fdVar
    case mb of
      Nothing  -> return ()       -- already released
      -- Closing the fd triggers the server's disconnect handling,
      -- which returns the token to the pool.
      Just fd' -> void $ try @IOException $ closeFd fd'
  return (SemaphoreToken fdVar)

releaseSemaphoreToken :: HasCallStack => SemaphoreToken -> IO ()
releaseSemaphoreToken (SemaphoreToken fdVar) =
  mask_ $ do
    mb <- tryTakeMVar fdVar
    case mb of
      Nothing -> return ()  -- already released
      Just fd -> do
        resp <- (do fdWriteByte fd CmdRelease
                    fdReadByte fd
                ) `finally` (void $ try @IOException $ closeFd fd)
        case resp of
          RspOk   -> return ()
          -- myCount <= 0 on the server means the token is effectively
          -- already released; stay idempotent.
          RspFail -> return ()
          _       -> fail "semaphore-compat: unexpected response in releaseSemaphoreToken"

-- On POSIX this is a no-op: 'ClientSemaphore' holds no live connection.
destroyClientSemaphore :: ClientSemaphore -> IO ()
destroyClientSemaphore _ = return ()

destroyServerSemaphore :: ServerSemaphore -> IO ()
destroyServerSemaphore server = uninterruptibleMask_ $ do
  -- we justify the uninterruptibleMask_ here by analysis of the server, which must exit
  -- in a bounded amount of time once it receives the cancel signal on serverCancelFd
  -- It has a bounded number of child threads it needs to cleanup before returning,
  -- but otherwise it should exit promptly.
  --
  -- Without uninterruptibleMask_, we potentially leak serverListenFd and path if an
  -- exception arrives when we are in `killThread`.
  mbState <- tryTakeMVar (serverState server)
  case mbState of
    Nothing -> return ()  -- already destroyed
    Just ServerState{..} -> do
      let path = semSocketPath $ serverClientSemaphore server
      -- Signal the server loop to exit pollAcceptSocket, then wait for it.
      void $ try @IOException $ fdWriteByte serverCancelFd 0
      void $ try @IOException $ closeFd serverCancelFd
      killThread (serverThreadId server)
      void $ try @IOException $ closeFd serverListenFd
      void $ try @IOException $ removeLink path

getSemaphoreValue :: ServerSemaphore -> IO Int
getSemaphoreValue server = readTVarIO (serverPool server)

get_time_seed :: IO Int
get_time_seed = do
  ns <- getMonotonicTimeNSec
  pid <- getProcessID
  return $ fromIntegral ns `xor` fromIntegral pid
