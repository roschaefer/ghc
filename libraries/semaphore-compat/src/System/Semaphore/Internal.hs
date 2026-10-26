{-# LANGUAGE CPP #-}

-- This module selects the internal implementation (POSIX, Windows, wasm/js)
-- with CPP and wraps it with user-facing types and documentation.
--
-- Benefits:
--
--  - All the documentation lives in a single place, instead of being duplicated
--    across every implementation.
--  - We can document platform-dependent behaviour, as opposed to e.g. the
--    Hackage page only showing POSIX-specific documentation.

-- | Platform-independent internal API.
module System.Semaphore.Internal
  ( ClientSemaphore(..), ServerSemaphore(..), SemaphoreToken(..)
  , clientSemaphoreName, serverClientSemaphore

  , waitOnSemaphore, tryWaitOnSemaphore
  , releaseSemaphoreToken

  , destroyClientSemaphore, destroyServerSemaphore

  , getSemaphoreValue

    -- * Internals
  , create_sem, open_sem_raw
  , Impl.get_time_seed
  ) where

-- base
import Data.Coerce ( coerce )
import GHC.Stack ( HasCallStack )

-- semaphore-compat
import System.Semaphore.Internal.Version

#if defined(wasm32_HOST_ARCH) || defined(javascript_HOST_ARCH)
import qualified System.Semaphore.Internal.Unsupported as Impl
#elif defined(mingw32_HOST_OS)
import qualified System.Semaphore.Internal.Win32 as Impl
#else
import qualified System.Semaphore.Internal.Posix as Impl
#endif

---------------------------------------
-- User-facing API

-- | A client-side semaphore: the handle a jobclient uses to acquire and release
-- tokens from a semaphore created by a jobserver.
--
-- Retrieve the underlying name with 'clientSemaphoreName'.
newtype ClientSemaphore = ClientSemaphore Impl.ClientSemaphore

-- | A server-side semaphore: a 'ClientSemaphore' together with the book-keeping
-- needed to track resource usage by all clients.
--
-- Retrieve the corresponding client semaphore with 'serverClientSemaphore'.
newtype ServerSemaphore = ServerSemaphore Impl.ServerSemaphore

-- | A held semaphore token: evidence of one unit of resource acquired from a
-- semaphore.
--
-- Use 'releaseSemaphoreToken' or 'withSemaphoreToken' to ensure prompt release
-- of semaphore tokens.
--
-- Platform-dependent behaviour:
--
--  - On POSIX, if all references to a 'SemaphoreToken' are dropped without
--    being released, a finalizer returns the token to the semaphore.
--  - On Windows, tokens held by a crashed client are permanently lost.
newtype SemaphoreToken = SemaphoreToken Impl.SemaphoreToken

-- | Retrieve the underlying name of a client-side semaphore.
clientSemaphoreName :: ClientSemaphore -> SemaphoreName
clientSemaphoreName = coerce Impl.clientSemaphoreName

-- | Retrieve the client-side semaphore corresponding to a server-side semaphore.
serverClientSemaphore :: ServerSemaphore -> ClientSemaphore
serverClientSemaphore = coerce Impl.serverClientSemaphore

-- | Acquire a token from the semaphore, blocking until one is available.
--
-- The returned 'SemaphoreToken' must be released with 'releaseSemaphoreToken'.
-- For prompt and predictable release of resources, callers should use
-- 'System.Semaphore.withSemaphoreToken' or 'releaseSemaphoreToken'.
--
-- This operation is interruptible: it can be cancelled by
-- 'Control.Concurrent.throwTo', 'Control.Concurrent.killThread', etc.
-- If interrupted, any transiently acquired token is automatically returned.
waitOnSemaphore :: HasCallStack => ClientSemaphore -> IO SemaphoreToken
waitOnSemaphore = coerce Impl.waitOnSemaphore

-- | Try to acquire a token from the semaphore without blocking.
--
-- Returns @Just token@ if a token was available, @Nothing@ otherwise.
--
-- This is __not__ an interruptible operation, but that should not be a problem:
-- this function is not expected to block for long, as the server is supposed
-- to respond immediately.
tryWaitOnSemaphore :: HasCallStack => ClientSemaphore -> IO (Maybe SemaphoreToken)
tryWaitOnSemaphore = coerce Impl.tryWaitOnSemaphore

-- | Release a semaphore token, returning it to the pool.
--
-- Idempotent: a second call on the same token is a safe no-op.
releaseSemaphoreToken :: HasCallStack => SemaphoreToken -> IO ()
releaseSemaphoreToken = coerce Impl.releaseSemaphoreToken

-- | Destroy a client-side semaphore.
--
-- This is an idempotent operation: double calls to 'destroyClientSemaphore'
-- do not cause any problems.
destroyClientSemaphore :: ClientSemaphore -> IO ()
destroyClientSemaphore = coerce Impl.destroyClientSemaphore

-- | Destroy a server-side semaphore.
--
-- Idempotent. Not interruptible.
destroyServerSemaphore :: ServerSemaphore -> IO ()
destroyServerSemaphore = coerce Impl.destroyServerSemaphore

-- | Query the current semaphore value (how many tokens it has available).
--
-- This is mainly for debugging use, as it is easy to introduce race conditions
-- when nontrivial program logic depends on the value returned by this function.
getSemaphoreValue :: ServerSemaphore -> IO Int
getSemaphoreValue = coerce Impl.getSemaphoreValue

--------------------------------------------------------------------------------
-- Internals

create_sem :: SemaphoreName -> Int -> IO (Either SemaphoreError ServerSemaphore)
create_sem = coerce Impl.create_sem

open_sem_raw :: SemaphoreName -> IO (Either SemaphoreError ClientSemaphore)
open_sem_raw = coerce Impl.open_sem_raw
