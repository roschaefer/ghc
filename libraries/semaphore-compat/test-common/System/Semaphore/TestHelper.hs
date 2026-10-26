{-# LANGUAGE CPP #-}

module System.Semaphore.TestHelper (
    -- Protocol
    Command(..), Response(..),
    encodeCommand, decodeCommand,
    encodeResponse, decodeResponse,
    -- Helper main loop (used by helper executable)
    helperMain,
    -- Test-side utilities (used by test suite)
    HelperHandle(..),
    withHelper,
    sendCommand, sendCommand_,
    sendCommandNoResponse,
    killHelper,
) where

import Control.Exception ( bracket, try, SomeException )
import Control.Monad ( when )
import Data.IORef ( IORef, newIORef, readIORef, writeIORef, modifyIORef' )
import Foreign.C.Types ( CInt(..) )
import System.Exit ( exitSuccess )
import System.IO
    ( Handle, hSetBuffering, hFlush, hGetLine, hPutStrLn, hClose
    , BufferMode(LineBuffering), stdin, stdout, hIsEOF )
import System.Process
    ( CreateProcess(..), StdStream(..), ProcessHandle
    , createProcess, proc, waitForProcess, getProcessExitCode
    , terminateProcess )

import System.Semaphore

#if !defined(mingw32_HOST_OS)
import System.Posix.Signals ( signalProcess, sigKILL )
import System.Process ( getPid )
#endif

--------------------------------------------------------------------------
-- Protocol types

data Command
    = CmdOpen String     -- ^ open <name>
    | CmdWait            -- ^ wait
    | CmdWaitN Int       -- ^ wait-n <n>
    | CmdRelease Int     -- ^ release <n>
    | CmdDestroy         -- ^ destroy
    | CmdCrash           -- ^ crash
    deriving (Show, Eq)

data Response
    = RspOk
    | RspInt Int
    | RspError String
    deriving (Show, Eq)

encodeCommand :: Command -> String
encodeCommand (CmdOpen name)  = "open " ++ name
encodeCommand CmdWait         = "wait"
encodeCommand (CmdWaitN n)    = "wait-n " ++ show n
encodeCommand (CmdRelease n)  = "release " ++ show n
encodeCommand CmdDestroy      = "destroy"
encodeCommand CmdCrash        = "crash"

decodeCommand :: String -> Maybe Command
decodeCommand "wait"    = Just CmdWait
decodeCommand "destroy" = Just CmdDestroy
decodeCommand "crash"   = Just CmdCrash
decodeCommand s
    | Just rest <- stripPrefix' "open " s
    = Just (CmdOpen rest)
    | Just rest <- stripPrefix' "wait-n " s
    , [(n, "")] <- reads rest
    = Just (CmdWaitN n)
    | Just rest <- stripPrefix' "release " s
    , [(n, "")] <- reads rest
    = Just (CmdRelease n)
    | otherwise
    = Nothing

encodeResponse :: Response -> String
encodeResponse RspOk          = "ok"
encodeResponse (RspInt n)     = "int " ++ show n
encodeResponse (RspError msg) = "error " ++ msg

decodeResponse :: String -> Maybe Response
decodeResponse "ok" = Just RspOk
decodeResponse s
    | Just rest <- stripPrefix' "int " s
    , [(n, "")] <- reads rest
    = Just (RspInt n)
    | Just rest <- stripPrefix' "error " s
    = Just (RspError rest)
    | otherwise
    = Nothing

stripPrefix' :: String -> String -> Maybe String
stripPrefix' [] ys = Just ys
stripPrefix' (_:_) [] = Nothing
stripPrefix' (x:xs) (y:ys)
    | x == y    = stripPrefix' xs ys
    | otherwise = Nothing

--------------------------------------------------------------------------
-- FFI for _exit (used by crash command)

foreign import ccall "stdlib.h _exit" c_exit :: CInt -> IO ()

--------------------------------------------------------------------------
-- Helper main loop

-- Helper state: semaphore identity + list of held tokens.
data HelperState = HelperState
  { hsSem    :: !(Maybe ClientSemaphore)
  , hsTokens :: ![SemaphoreToken]
  }

helperMain :: IO ()
helperMain = do
    hSetBuffering stdin LineBuffering
    hSetBuffering stdout LineBuffering
    ref <- newIORef (HelperState Nothing [])
    helperLoop ref

helperLoop :: IORef HelperState -> IO ()
helperLoop ref = do
    eof <- hIsEOF stdin
    if eof
        then do
            -- Clean exit on EOF: release all tokens and destroy semaphore
            st <- readIORef ref
            mapM_ releaseSemaphoreToken (hsTokens st)
            case hsSem st of
                Just sem -> destroyClientSemaphore sem
                Nothing  -> return ()
            exitSuccess
        else do
            line <- hGetLine stdin
            case decodeCommand line of
                Nothing -> do
                    respond (RspError ("unknown command: " ++ line))
                    helperLoop ref
                Just cmd -> do
                    executeCommand ref cmd

executeCommand :: IORef HelperState -> Command -> IO ()
executeCommand ref cmd = case cmd of
    CmdOpen name -> do
        result <- openSemaphore name
        case result of
            Right sem -> do
                modifyIORef' ref (\st -> st { hsSem = Just sem })
                respond RspOk
            Left err ->
                respond (RspError (show err))
        helperLoop ref

    CmdWait -> do
        st <- readIORef ref
        case hsSem st of
            Nothing -> do
                respond (RspError "no semaphore open")
                helperLoop ref
            Just sem -> do
                tok <- waitOnSemaphore sem
                writeIORef ref (st { hsTokens = tok : hsTokens st })
                respond RspOk
                helperLoop ref

    CmdWaitN n -> do
        st <- readIORef ref
        case hsSem st of
            Nothing -> do
                respond (RspError "no semaphore open")
                helperLoop ref
            Just sem -> do
                toks <- sequence [ waitOnSemaphore sem | _ <- [1..n] ]
                writeIORef ref (st { hsTokens = toks ++ hsTokens st })
                respond RspOk
                helperLoop ref

    CmdRelease n -> do
        st <- readIORef ref
        let (toRelease, toKeep) = splitAt n (hsTokens st)
        mapM_ releaseSemaphoreToken toRelease
        writeIORef ref (st { hsTokens = toKeep })
        respond RspOk
        helperLoop ref

    CmdDestroy -> do
        st <- readIORef ref
        mapM_ releaseSemaphoreToken (hsTokens st)
        case hsSem st of
            Just sem -> destroyClientSemaphore sem
            Nothing  -> return ()
        writeIORef ref (HelperState Nothing [])
        respond RspOk
        exitSuccess

    CmdCrash -> do
        c_exit 0

respond :: Response -> IO ()
respond r = do
    hPutStrLn stdout (encodeResponse r)
    hFlush stdout

--------------------------------------------------------------------------
-- Test-side: HelperHandle and utilities

data HelperHandle = HelperHandle
    { hStdin   :: Handle
    , hStdout  :: Handle
    , hStderr  :: Handle
    , hProcess :: ProcessHandle
    }

startHelper :: IO HelperHandle
startHelper = do
    (Just hin, Just hout, Just herr, ph) <- createProcess (proc "semaphore-helper" [])
        { std_in  = CreatePipe
        , std_out = CreatePipe
        , std_err = CreatePipe
        }
    hSetBuffering hin LineBuffering
    hSetBuffering hout LineBuffering
    return (HelperHandle hin hout herr ph)

cleanupHelper :: HelperHandle -> IO ()
cleanupHelper h = do
    ec <- getProcessExitCode (hProcess h)
    when (ec == Nothing) $ do
        _ <- try (terminateProcess (hProcess h)) :: IO (Either SomeException ())
        _ <- waitForProcess (hProcess h)
        return ()
    _ <- try (hClose (hStdin h)) :: IO (Either SomeException ())
    _ <- try (hClose (hStdout h)) :: IO (Either SomeException ())
    _ <- try (hClose (hStderr h)) :: IO (Either SomeException ())
    return ()

withHelper :: (HelperHandle -> IO a) -> IO a
withHelper = bracket startHelper cleanupHelper

-- | Send a command and wait for a response.
sendCommand :: HelperHandle -> Command -> IO Response
sendCommand h cmd = do
    hPutStrLn (hStdin h) (encodeCommand cmd)
    hFlush (hStdin h)
    line <- hGetLine (hStdout h)
    case decodeResponse line of
        Just r  -> return r
        Nothing -> return (RspError ("bad response: " ++ line))

-- | Send a command and expect 'RspOk'. Throws on error.
sendCommand_ :: HelperHandle -> Command -> IO ()
sendCommand_ h cmd = do
    r <- sendCommand h cmd
    case r of
        RspOk -> return ()
        _     -> error ("sendCommand_: expected ok, got " ++ show r)

-- | Send a command but don't read a response (for blocking commands like wait).
sendCommandNoResponse :: HelperHandle -> Command -> IO ()
sendCommandNoResponse h cmd = do
    hPutStrLn (hStdin h) (encodeCommand cmd)
    hFlush (hStdin h)

-- | Kill the helper process with SIGKILL (Unix) or terminateProcess (Windows).
killHelper :: HelperHandle -> IO ()
killHelper h = do
#if !defined(mingw32_HOST_OS)
    mbPid <- getPid (hProcess h)
    case mbPid of
        Just pid -> signalProcess sigKILL pid
        Nothing  -> terminateProcess (hProcess h)
#else
    terminateProcess (hProcess h)
#endif
    _ <- waitForProcess (hProcess h)
    return ()
