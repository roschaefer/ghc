{-# LANGUAGE ForeignFunctionInterface #-}

-- | FFI glue code for Unix domain sockets.
module System.Semaphore.Internal.DomainSocket
  ( connectDomainSocket
  , listenDomainSocket
  , pollAcceptSocket, AcceptResult(..)
  , fdReadByte, fdWriteByte
  , fdShutdown
  ) where

-- base
import Data.Word ( Word8 )
import Foreign.C.Error ( throwErrnoIfMinus1Retry, throwErrnoIfMinus1Retry_, throwErrno )
import Foreign.C.String ( CString, withCString )
import Foreign.C.Types ( CInt(..), CSize(..) )
import Foreign.Marshal.Alloc ( allocaBytes )
import Foreign.Ptr ( Ptr )
import Foreign.Storable ( peek, poke )
import GHC.IO.Exception ( IOErrorType(EOF), IOException(..) )
import GHC.Stack ( HasCallStack, callStack, prettyCallStack )

-- unix
import System.Posix.Types ( Fd(..) )

--------------------------------------------------------------------------------

foreign import ccall safe "hs_connect_domain_socket"
    c_connectDomainSocket :: CString -> IO CInt

connectDomainSocket :: FilePath -> IO Fd
connectDomainSocket path =
    withCString path $ fmap Fd . throwErrnoIfMinus1Retry "connectDomainSocket" . c_connectDomainSocket

foreign import ccall safe "hs_listen_domain_socket"
    c_listenDomainSocket :: CString -> IO CInt

-- | Open a socket in non blocking mode (O_NONBLOCK)
listenDomainSocket :: FilePath -> IO Fd
listenDomainSocket path =
    withCString path $ fmap Fd . throwErrnoIfMinus1Retry "listenDomainSocket" . c_listenDomainSocket

foreign import ccall safe "read"
    c_read :: CInt -> Ptr Word8 -> CSize -> IO CInt

foreign import ccall safe "write"
    c_write :: CInt -> Ptr Word8 -> CSize -> IO CInt

-- | Read a single byte from a file descriptor.
-- Throws an EOF 'IOError' if the peer has disconnected.
fdReadByte :: HasCallStack => Fd -> IO Word8
fdReadByte (Fd fd) =
    allocaBytes 1 $ \buf -> do
        rc <- throwErrnoIfMinus1Retry ("fdReadByte(fd=" ++ show fd ++ ")") $
            c_read fd buf 1
        if rc == 0
          then ioError $ IOError Nothing EOF
                  (prettyCallStack callStack)
                  ("fd=" ++ show fd)
                  Nothing Nothing
          else peek buf

-- | Write a single byte to a file descriptor.
fdWriteByte :: HasCallStack => Fd -> Word8 -> IO ()
fdWriteByte (Fd fd) byte =
    allocaBytes 1 $ \buf -> do
        poke buf byte
        _ <- throwErrnoIfMinus1Retry ("fdWriteByte(fd=" ++ show fd ++ ")") $
            c_write fd buf 1
        return ()

foreign import ccall safe "shutdown"
    c_shutdown :: CInt -> CInt -> IO CInt

-- | Shut down a socket for both reading and writing.
-- A concurrent 'fdReadByte' on the same fd will return immediately
-- Used to cancel threads blocked in read()
fdShutdown :: Fd -> IO ()
fdShutdown (Fd fd) =
    throwErrnoIfMinus1Retry_ "fdShutdown" $ c_shutdown fd 2  -- SHUT_RDWR

-- | Result of 'pollAcceptSocket'.
data AcceptResult
  = AcceptedFd !Fd     -- ^ A client connected; here is the fd.
  | AcceptCancelled    -- ^ The cancel pipe was signalled.

foreign import ccall safe "hs_poll_accept"
    c_pollAccept :: CInt -> CInt -> IO CInt

-- | Block until either a client connects or the cancel fd is written to.
--
-- Relies on cooperative cancellation implemented in hs_poll_accept using
-- @poll(2)@ + @accept(2)@ via safe FFI to avoid GHC #27110 and #27113.
--
-- Must be called from a masked context.  The caller is responsible for
-- installing an exception handler that closes all 3 fds (inputs and outputs).
pollAcceptSocket :: Fd -> Fd -> IO AcceptResult
pollAcceptSocket (Fd listenFd) (Fd cancelFd) = do
    r <- c_pollAccept listenFd cancelFd
    if r == -2
      then return AcceptCancelled
      else if r == -1
        then throwErrno "pollAcceptSocket"
        else return (AcceptedFd (Fd r))
