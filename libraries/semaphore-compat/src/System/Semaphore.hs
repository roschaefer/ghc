
-- | This library provides a cross-platform implementation of semaphores.
--
-- Its main role is to provide a cross-platform notion of semaphore that can be
-- used to implement the jobserver\/jobclient model of
-- [GHC proposal #540](https://ghc-proposals.readthedocs.io/en/latest/proposals/0540-jsem.html).
--
-- Typical usage:
--
--  - The jobserver creates a 'ServerSemaphore', e.g. using 'createSemaphore'/'freshSemaphore'.
--  - The jobserver retrieves the serialisable 'SemaphoreIdentifier' of this
--    'ServerSemaphore' by using 'serverClientSemaphore', 'clientSemaphoreName'
--    and 'semaphoreIdentifier'.
--  - The jobserver passes the 'SemaphoreIdentifier' to the jobclient via IPC.
--  - The jobclient uses 'openSemaphore' to obtain the 'ClientSemaphore'
--    corresponding to the 'SemaphoreIdentifier' it has been given.
--  - The jobclient can then request resources from the semaphore using
--    'waitOnSemaphore' (and return them using 'releaseSemaphoreToken'), or by
--    using the 'withAbstractSem' bracket pattern.
--  - Once the jobclient is done, it uses 'destroyClientSemaphore' to ensure
--    all resources are released (including any remaining tokens).
--  - Once the jobserver is done, it cleans up the resources underlying the
--    semaphore using 'destroyServerSemaphore'.
--
-- NB: the above usage outline deliberately omits any mention of implicit
-- semaphore tokens: such a notion does not exist in this library, as they are
-- purely an abstraction layer used by the jobserver implementation.
-- See 'destroyClientSemaphore' for more details.
module System.Semaphore
  ( -- * System semaphores

    -- $server-vs-client

    ClientSemaphore, ServerSemaphore
  , serverClientSemaphore

  , SemaphoreName(..)
  , clientSemaphoreName

  , SemaphoreIdentifier
  , semaphoreIdentifier

    -- ** Creating a semaphore
  , createSemaphore, freshSemaphore

    -- ** Opening a semaphore
  , SemaphoreToken
  , openSemaphore
  , parseSemaphoreIdentifier
  , SemaphoreProtocolVersion(..)
  , semaphoreVersion
  , versionsAreCompatible
  , SemaphoreError(..)

    -- ** Requesting a token
  , waitOnSemaphore, tryWaitOnSemaphore
  , withSemaphoreToken
  , getSemaphoreValue

    -- ** Releasing resources
  , releaseSemaphoreToken

  , destroyClientSemaphore, destroyServerSemaphore
    -- $implicit-tokens

  -- * Abstract semaphores
  , AbstractSem(..)
  , withAbstractSem
  ) where

-- base
import Control.Exception ( bracket, bracket_ )
import Data.List.NonEmpty ( NonEmpty(..) )

-- semaphore-compat
import System.Semaphore.Internal
import System.Semaphore.Internal.Version

--------------------------------------------------------------------------------

{- $server-vs-client

Since version 2 of @semaphore-compat@, the library distinguishes between two
kinds of semaphores:

  - When a jobserver creates a semaphore via 'createSemaphore', it obtains
    a 'ServerSemaphore'.
  - Jobclients, which are passed the 'SemaphoreIdentifier' of a pre-existing
    semaphore, obtain a 'ClientSemaphore' via 'openSemaphore'.

When the jobserver wants to also act as a jobclient, it can use
'serverClientSemaphore' to obtain the 'ClientSemaphore' corresponding
to its 'ServerSemaphore'.

This architecture allows the jobserver to keep full accounting of
semaphore resources held by all clients.
-}

{- $implicit-tokens

The GHC jobserver protocol described in [GHC proposal #540](https://ghc-proposals.readthedocs.io/en/latest/proposals/0540-jsem.html)
specifies the notion of __implicit semaphore tokens__.

  1. To invoke a jobclient, the jobserver must be in possession of at least one
     semaphore token. We consider that the jobserver passes on ownership of this
     token to the jobclient, even though no interaction with the semaphore takes
     place. This is the __implicit token__.
  2. Each jobclient thus starts with one available token (the implicit token),
     and can request more tokens by waiting on the semaphore.

A jobclient calling 'destroyClientSemaphore' signals to the jobserver that the
client is done doing work. The jobserver thus regains control of the implicit
token, which it can now use to spawn other jobclients.

Implicit tokens are purely an accounting abstraction that we superimpose on top
of semaphore token ownership for the purposes of implementing the jobserver
protocol. They are not part of this library.
-}

---------------------------------------
-- Version compatibility

-- | Check whether two semaphore protocol versions are compatible.
versionsAreCompatible :: SemaphoreProtocolVersion -> SemaphoreProtocolVersion -> Bool
versionsAreCompatible a b =
  -- For now, only identical versions are compatible, but keeping this
  -- check abstract (via 'versionsAreCompatible') gives us more flexibility
  -- when evolving the library.
  a == b

---------------------------------------
-- System-specific semaphores

-- | Create a new semaphore with the given label and initial token count.
createSemaphore :: String -- ^ label
                -> Int    -- ^ number of tokens on the semaphore
                -> IO (Either SemaphoreError ServerSemaphore)
createSemaphore label init_toks = do
  let sem_nm = SemaphoreName
        { semaphoreProtocolVersion = semaphoreVersion
        , unversionedSemaphoreNameString = label
        }
  create_sem sem_nm init_toks

-- | Create a fresh semaphore with a unique name and the given token count.
--
-- The name is derived from the given prefix with a random suffix.
freshSemaphore :: String -- ^ label prefix
               -> Int    -- ^ number of tokens on the semaphore
               -> IO (Either SemaphoreError ServerSemaphore)
freshSemaphore prefix init_toks = do
  seed <- get_time_seed
  go 0 (seedStrings seed)
  where
    go :: Int -> NonEmpty String -> IO (Either SemaphoreError ServerSemaphore)
    go i (suffix :| suffs) = do
      let sem_str = prefix ++ "_" ++ suffix
          sem_nm  = SemaphoreName
            { semaphoreProtocolVersion = semaphoreVersion
            , unversionedSemaphoreNameString = sem_str
            }
      mb_sem <- create_sem sem_nm init_toks
      case mb_sem of
        Right sem -> return (Right sem)
        Left  err
          | next : nexts <- suffs
          , i < 32 -- give up after 32 attempts
          -> go (i+1) (next :| nexts)
          | otherwise
          -> return (Left err)

-- | Open a pre-existing semaphore.
--
-- Returns @Left SemaphoreIncompatibleVersion@ if the semaphore protocol
-- version is not compatible with the current version of @semaphore-compat@.
openSemaphore :: SemaphoreIdentifier -> IO (Either SemaphoreError ClientSemaphore)
openSemaphore ident =
  case parseSemaphoreIdentifier ident of
    Nothing
      | versionsAreCompatible v1 semaphoreVersion ->
          open_sem_raw (SemaphoreName { semaphoreProtocolVersion = v1
                                      , unversionedSemaphoreNameString = ident })
      | otherwise ->
          return $ Left $ semVerError v1
    Just nm
      | not (versionsAreCompatible (semaphoreProtocolVersion nm) semaphoreVersion) ->
          return $ Left $ semVerError (semaphoreProtocolVersion nm)
      | otherwise ->
          open_sem_raw nm
  where
    v1 = SemaphoreProtocolVersion 1
    semVerError ver = SemaphoreIncompatibleVersion ver semaphoreVersion

-- | Acquire a token, run an action, then release the token. Exception safe.
withSemaphoreToken :: ClientSemaphore -> (SemaphoreToken -> IO a) -> IO a
withSemaphoreToken sem = bracket (waitOnSemaphore sem) releaseSemaphoreToken

seedStrings :: Int -> NonEmpty String
seedStrings seed = fmap ( \ i -> iToBase62 (i + seed) ) (0 :| [1..])

---------------------------------------
-- Abstract semaphores

-- | Abstraction over the operations of a semaphore.
data AbstractSem =
  AbstractSem
    { acquireSem :: IO ()
    , releaseSem :: IO ()
    }

-- | Acquire/release bracket pattern: acquire a token, perform an action,
-- and release the token.
--
-- Guarantees that the token is released in case that the inner action throws
-- an exception.
withAbstractSem :: AbstractSem -> IO b -> IO b
withAbstractSem s = bracket_ (acquireSem s) (releaseSem s)
