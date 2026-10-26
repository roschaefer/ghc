{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE LambdaCase #-}

-- | Stub backend for wasm and targets without POSIX/Win32 sockets/semaphores.
module System.Semaphore.Internal.Unsupported
  ( ClientSemaphore, ServerSemaphore
  , SemaphoreToken
  , clientSemaphoreName, serverClientSemaphore
  , waitOnSemaphore, tryWaitOnSemaphore
  , releaseSemaphoreToken
  , destroyClientSemaphore, destroyServerSemaphore
  , getSemaphoreValue

  , create_sem, open_sem_raw
  , get_time_seed
  ) where

-- base
import Control.Exception ( IOException )
import GHC.IO.Exception ( unsupportedOperation )
import System.IO.Error ( ioeSetLocation )

-- semaphore-compat
import System.Semaphore.Internal.Version

--------------------------------------------------------------------------------

data ClientSemaphore

data ServerSemaphore

data SemaphoreToken

clientSemaphoreName :: ClientSemaphore -> SemaphoreName
clientSemaphoreName = \case {}

serverClientSemaphore :: ServerSemaphore -> ClientSemaphore
serverClientSemaphore = \case {}

unsupported :: String -> IOException
unsupported op = ioeSetLocation unsupportedOperation ("semaphore-compat." ++ op)

create_sem :: SemaphoreName -> Int -> IO (Either SemaphoreError ServerSemaphore)
create_sem _ _ =
  return $ Left $ SemaphoreOtherError $ unsupported "createSemaphore"

open_sem_raw :: SemaphoreName -> IO (Either SemaphoreError ClientSemaphore)
open_sem_raw _ =
  return $ Left $ SemaphoreOtherError $ unsupported "openSemaphore"

waitOnSemaphore :: ClientSemaphore -> IO SemaphoreToken
waitOnSemaphore = \case {}

tryWaitOnSemaphore :: ClientSemaphore -> IO (Maybe SemaphoreToken)
tryWaitOnSemaphore = \case {}

releaseSemaphoreToken :: SemaphoreToken -> IO ()
releaseSemaphoreToken = \case {}

destroyClientSemaphore :: ClientSemaphore -> IO ()
destroyClientSemaphore = \case {}

destroyServerSemaphore :: ServerSemaphore -> IO ()
destroyServerSemaphore = \case {}

getSemaphoreValue :: ServerSemaphore -> IO Int
getSemaphoreValue = \case {}

get_time_seed :: IO Int
get_time_seed = ioError $ unsupported "get_time_seed"
