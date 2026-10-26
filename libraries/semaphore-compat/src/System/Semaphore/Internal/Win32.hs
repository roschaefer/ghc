{-# LANGUAGE TypeApplications #-}

-- | Windows backend: semaphores implemented on top of Win32 named semaphores.
module System.Semaphore.Internal.Win32
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
  ( forkIO, killThread )
import Control.Concurrent.MVar
  ( MVar, newEmptyMVar, newMVar, putMVar, takeMVar, tryReadMVar, tryTakeMVar )
import Control.Exception
  ( IOException, SomeException
  , finally, mask_, onException, throw, try, uninterruptibleMask_
  )
import Control.Monad
import System.IO.Error ( isDoesNotExistError )

-- win32
import qualified System.Win32.Event     as Win32
  ( createEvent, setEvent
  , waitForSingleObject, waitForMultipleObjects
  , wAIT_OBJECT_0 )
import qualified System.Win32.File      as Win32
  ( closeHandle )
import qualified System.Win32.Process   as Win32
  ( iNFINITE )
import qualified System.Win32.Semaphore as Win32
  ( Semaphore(..), sEMAPHORE_ALL_ACCESS
  , createSemaphore, openSemaphore, releaseSemaphore )
import qualified System.Win32.Time      as Win32
  ( FILETIME(..), getSystemTimeAsFileTime )

-- semaphore-compat
import System.Semaphore.Internal.Version

--------------------------------------------------------------------------------

data ClientSemaphore =
  ClientSemaphore
    { clientSemaphoreName :: !SemaphoreName
    , semaphore           :: !(MVar Win32.Semaphore)
      -- NB: the MVar is emptied when the handle is closed to prevent double close.
    }

newtype SemaphoreToken = SemaphoreToken
  { tokenSemaphore :: MVar Win32.Semaphore
    -- NB: the MVar is emptied when the token is released to prevent double release.
  }

newtype ServerSemaphore = ServerSemaphore
  { serverClientSemaphore :: ClientSemaphore
  }

create_sem :: SemaphoreName -> Int -> IO (Either SemaphoreError ServerSemaphore)
create_sem sem_nm init_toks = mask_ $ do
  let toks = fromIntegral init_toks
  mb_sem <- try @IOException $
    Win32.createSemaphore Nothing toks 0x7FFFFFFF (Just (semaphoreIdentifier sem_nm))
  case mb_sem of
    Left err ->
      return $ Left $ SemaphoreOtherError err
    Right (sem, True) -> do
      -- Semaphore already existed; createSemaphore still returned a valid
      -- handle to it, which we must close to avoid leaking it.
      void $ try @IOException $ Win32.closeHandle (Win32.semaphoreHandle sem)
      return $ Left $ SemaphoreAlreadyExists (semaphoreIdentifier sem_nm)
    Right (sem, False) -> do
      semVar <- newMVar sem
      return $ Right $
        ServerSemaphore
          { serverClientSemaphore =
              ClientSemaphore
                { clientSemaphoreName = sem_nm
                , semaphore           = semVar
                }
          }

open_sem_raw :: SemaphoreName -> IO (Either SemaphoreError ClientSemaphore)
open_sem_raw nm = mask_ $ do
  mb_res <- try @IOException $
    Win32.openSemaphore Win32.sEMAPHORE_ALL_ACCESS True (semaphoreIdentifier nm)
  case mb_res of
    Left  err
      | isDoesNotExistError err
      -> return $ Left $ SemaphoreDoesNotExist (semaphoreIdentifier nm)
      | otherwise
      -> return $ Left $ SemaphoreOtherError err
    Right sem -> do
      semVar <- newMVar sem
      return $ Right $
        ClientSemaphore
          { clientSemaphoreName = nm
          , semaphore           = semVar
          }

-- Read the kernel handle from a 'ClientSemaphore'.  Throws if the
-- client has been destroyed.
readClientHandle :: ClientSemaphore -> String -> IO Win32.Semaphore
readClientHandle client opName = do
  mb <- tryReadMVar (semaphore client)
  case mb of
    Just sem -> return sem
    Nothing  -> ioError $ userError $
      "semaphore-compat." ++ opName ++ ": semaphore has been destroyed"

waitOnSemaphore :: ClientSemaphore -> IO SemaphoreToken
waitOnSemaphore client = do
  resultVar <- newEmptyMVar
  mask_ $ do
    sem <- readClientHandle client "waitOnSemaphore"
    cancelEvent <- Win32.createEvent Nothing True False ""
    workerTid <- (forkIO $ mask_ $ do
      res <- try @SomeException $ do
        wait_res <-
          Win32.waitForMultipleObjects
            [ Win32.semaphoreHandle sem
            , cancelEvent ]
            False -- WaitAny
            Win32.iNFINITE
        if wait_res == Win32.wAIT_OBJECT_0
          then SemaphoreToken <$> newMVar sem
          else ioError (userError "semaphore wait cancelled")
      putMVar resultVar res
      ) `onException` Win32.closeHandle cancelEvent
    let cleanup =
          -- Signal the cancel event and wait for the worker to exit.
          (do ok <- try @IOException $ Win32.setEvent cancelEvent
              case ok of
                Right () ->
                  -- Cancel event signaled. Worker's WaitForMultipleObjects
                  -- returns immediately. Safe to wait uninterruptibly.
                  uninterruptibleMask_ $ killThread workerTid
                Left _ ->
                  -- setEvent failed. We can't really do anything.
                  -- The worker is in a safe ffi call so killThread
                  -- will just block forever.
                  return ()
          ) `finally` (do
              -- Recover any transiently acquired token.  If the worker
              -- acquired the semaphore before seeing the cancel event,
              -- release it back to the pool.
              mbRes <- tryTakeMVar resultVar
              case mbRes of
                Just (Right tok) ->
                  void $ try @SomeException $ releaseSemaphoreToken tok
                _ -> return ()
              Win32.closeHandle cancelEvent)
    res <- takeMVar resultVar `onException` cleanup
    Win32.closeHandle cancelEvent
    case res of
      Right tok -> return tok
      Left e    -> throw e

tryWaitOnSemaphore :: ClientSemaphore -> IO (Maybe SemaphoreToken)
tryWaitOnSemaphore client =
  mask_ $ do
    sem <- readClientHandle client "tryWaitOnSemaphore"
    wait_res <- Win32.waitForSingleObject (Win32.semaphoreHandle sem) 0
    if wait_res == Win32.wAIT_OBJECT_0
      then Just . SemaphoreToken <$> newMVar sem
      else return Nothing

releaseSemaphoreToken :: SemaphoreToken -> IO ()
releaseSemaphoreToken (SemaphoreToken v) =
  mask_ $ do
    mb <- tryTakeMVar v
    case mb of
      Nothing  -> return ()
      Just sem -> void $ Win32.releaseSemaphore sem 1

destroyClientSemaphore :: ClientSemaphore -> IO ()
destroyClientSemaphore client = uninterruptibleMask_ $ do
  mb <- tryTakeMVar (semaphore client)
  case mb of
    Nothing  -> return ()  -- already destroyed
    Just sem -> Win32.closeHandle (Win32.semaphoreHandle sem)

destroyServerSemaphore :: ServerSemaphore -> IO ()
destroyServerSemaphore server =
  destroyClientSemaphore (serverClientSemaphore server)

getSemaphoreValue :: ServerSemaphore -> IO Int
getSemaphoreValue server =
  mask_ $ do
    sem <- readClientHandle (serverClientSemaphore server) "getSemaphoreValue"
    wait_res <- Win32.waitForSingleObject (Win32.semaphoreHandle sem) 0
    if wait_res == Win32.wAIT_OBJECT_0
    then
      (+1) . fromIntegral <$> Win32.releaseSemaphore sem 1
    else
      return 0

get_time_seed :: IO Int
get_time_seed = do
  Win32.FILETIME t <- Win32.getSystemTimeAsFileTime
  return (fromIntegral t)
