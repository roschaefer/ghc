{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}

-- | Semaphore protocol version and versioned semaphore identifiers.
module System.Semaphore.Internal.Version
  ( SemaphoreName(..)
  , SemaphoreIdentifier
  , SemaphoreProtocolVersion(..)
  , SemaphoreError(..)
  , semaphoreVersion
  , semaphoreIdentifier
  , parseSemaphoreIdentifier
  , getSemaphoreSocketPath
  , iToBase62
  ) where

-- base
import Control.Exception ( Exception, IOException )
import GHC.Exts ( Char(..), Int(..), indexCharOffAddr# )

-- directory
import System.Directory ( getTemporaryDirectory, createDirectoryIfMissing )

-- filepath
import System.FilePath ( (</>) )

---------------------------------------
-- Types

-- | The semaphore protocol version currently being used.
--
-- The version tracks the IPC mechanism, not the library version:
--
--   * __POSIX:__ 2 (domain sockets, replacing v1 system semaphores).
--   * __Windows:__ 1 (Win32 named semaphores, unchanged from v1).
--   * __Unsupported platforms:__ 0 (no compatible IPC backend).
newtype SemaphoreProtocolVersion =
  SemaphoreProtocolVersion { getSemaphoreProtocolVersion :: Int }
  deriving (Eq, Ord, Show)

-- | A semaphore name: a protocol version and an unversioned name string.
data SemaphoreName = SemaphoreName
  { semaphoreProtocolVersion :: !SemaphoreProtocolVersion
  , unversionedSemaphoreNameString :: !String
  } deriving (Eq, Show)

-- | The identifier string of a semaphore: a serialised 'SemaphoreName' used
-- for inter-process communication (e.g. as a command line argument).
--
-- The specific format of this name string depends on the __protocol__ version:
--
--   - For version 1, this is the unadorned semaphore name.
--   - For version @N >= 2@, the semaphore name is prefixed with @v\<N\>-@.
type SemaphoreIdentifier = String

-- | Errors that can occur when creating or opening a semaphore.
data SemaphoreError
  -- | Can't create a semaphore: a semaphore with this name already exists.
  = SemaphoreAlreadyExists
    { semaphoreErrorIdentifier :: !SemaphoreIdentifier
    }
  -- | Can't open a semaphore: no semaphore with this name exists.
  | SemaphoreDoesNotExist
    { semaphoreErrorIdentifier :: !SemaphoreIdentifier
    }
  -- | Protocol version incompatibility: the semaphore identifier uses a
  -- different protocol version than the library is currently using.
  | SemaphoreIncompatibleVersion
    { semaphoreErrorActualVersion   :: !SemaphoreProtocolVersion
    , semaphoreErrorExpectedVersion :: !SemaphoreProtocolVersion
    }
  -- | An 'IOException' was raised when creating or opening the semaphore.
  | SemaphoreOtherError
    { semaphoreOtherError :: !IOException
    }
  deriving (Show, Eq)

instance Exception SemaphoreError

---------------------------------------
-- Version negotiation

-- | The protocol version used on this host platform with this version of
-- @semaphore-compat@.
--
-- See 'SemaphoreProtocolVersion'.
semaphoreVersion :: SemaphoreProtocolVersion
#if defined(mingw32_HOST_OS)
semaphoreVersion = SemaphoreProtocolVersion 1
#elif defined(wasm32_HOST_ARCH) || defined(javascript_HOST_ARCH)
semaphoreVersion = SemaphoreProtocolVersion 0
#else
semaphoreVersion = SemaphoreProtocolVersion 2
#endif


-- | The serialised identifier of a 'SemaphoreName' for inter-process
-- communication.
--
-- See 'SemaphoreIdentifier' for more information.
semaphoreIdentifier :: SemaphoreName -> SemaphoreIdentifier
semaphoreIdentifier sn
  | ver <= 1  = unversionedSemaphoreNameString sn
  | otherwise = "v" ++ show ver ++ "-" ++ unversionedSemaphoreNameString sn
  where
    ver = getSemaphoreProtocolVersion (semaphoreProtocolVersion sn)



-- TODO: I think 'parseSemaphoreIdentifier' is unused (neither Cabal nor GHC use it).

-- | Parse a 'SemaphoreIdentifier' into a 'SemaphoreName'.
--
-- Returns @Nothing@ for unversioned strings (which should be treated as
-- v1 by the caller's compatibility logic).
parseSemaphoreIdentifier :: SemaphoreIdentifier -> Maybe SemaphoreName
parseSemaphoreIdentifier ('v':rest) =
  case reads rest of
    [(n, '-':name)] -> Just (SemaphoreName (SemaphoreProtocolVersion n) name)
    _               -> Nothing
parseSemaphoreIdentifier _ = Nothing

---------------------------------------
-- Utilities

-- | Convert an 'Int' to a base-62 string (digits @0@–@9@, @a@–@z@, @A@–@Z@).
iToBase62 :: Int -> String
iToBase62 m = go m' ""
  where
    m'
      | m == minBound = maxBound
      | otherwise     = abs m
    go n cs | n < 62
            = let !c = chooseChar62 n
              in c : cs
            | otherwise
            = let !(!q, r) = quotRem n 62
                  !c       = chooseChar62 r
              in go q (c : cs)

    chooseChar62 :: Int -> Char
    {-# INLINE chooseChar62 #-}
    chooseChar62 (I# n) = C# (indexCharOffAddr# chars62 n)
    chars62 = "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"#

-- | The socket file path for a semaphore.
getSemaphoreSocketPath :: SemaphoreName -> IO FilePath
getSemaphoreSocketPath sn = do
  tmp <- getTemporaryDirectory
  let dir = tmp </> "semaphore-compat"
  createDirectoryIfMissing True dir
  return (dir </> unversionedSemaphoreNameString sn)
