{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Control.Concurrent
  ( forkIO, killThread, myThreadId, threadDelay
  , newEmptyMVar, putMVar, takeMVar )
import Control.Exception
  ( SomeException, AsyncException(ThreadKilled)
  , bracket, mask, mask_, throwTo, try )
import System.Exit ( ExitCode )
import System.Mem ( performMajorGC )
import Control.Monad
  ( void, forM_, replicateM, replicateM_ )
import Data.IORef
  ( IORef, newIORef, readIORef, writeIORef, atomicModifyIORef' )
import Data.List ( isPrefixOf )
import System.Process
    ( waitForProcess )
import System.Timeout ( timeout )

import Test.Tasty
import Test.Tasty.ExpectedFailure (expectFailBecause)
import Test.Tasty.HUnit

import System.Semaphore
import qualified System.Semaphore.Internal.Version as Internal
import System.Semaphore.TestHelper

import System.Semaphore.Internal ( SemaphoreToken(..) )
#if !defined(mingw32_HOST_OS)
import Control.Concurrent.MVar ( tryTakeMVar )
import System.Semaphore.Internal.Posix ( tokenFdLock )
import System.Semaphore.Internal.DomainSocket
  ( connectDomainSocket, fdReadByte, fdWriteByte )
import System.Posix.IO ( closeFd )
#endif

-- | Simulate a crash: close the token's underlying connection without
-- sending a release command.  The server should detect the disconnect
-- and return the token to the pool.
crashToken :: SemaphoreToken -> IO ()
#if !defined(mingw32_HOST_OS)
crashToken (SemaphoreToken tok) = do
  mb <- tryTakeMVar (tokenFdLock tok)
  case mb of
    Just fd -> closeFd fd
    Nothing -> return ()  -- already released
#else
crashToken _tok = return () -- Win32: no fd to close; crash recovery not supported
#endif

-- | Shorthand for constructing 'SemaphoreProtocolVersion' in tests.
v :: Int -> SemaphoreProtocolVersion
v = SemaphoreProtocolVersion

--------------------------------------------------------------------------
-- Test helpers

unwrapServer :: Either SemaphoreError ServerSemaphore -> IO ServerSemaphore
unwrapServer (Right srv) = return srv
unwrapServer (Left  err) = assertFailure ("unexpected SemaphoreError: " ++ show err)
                            >> error "unreachable"

unwrapSem :: Either SemaphoreError ClientSemaphore -> IO ClientSemaphore
unwrapSem (Right sem) = return sem
unwrapSem (Left  err) = assertFailure ("unexpected SemaphoreError: " ++ show err)
                         >> error "unreachable"

withSemaphore :: String -> Int -> (ServerSemaphore -> IO a) -> IO a
withSemaphore _label toks =
  bracket (freshSemaphore "sem_test" toks >>= unwrapServer)
    destroyServerSemaphore

withFreshSemaphore :: Int -> (ServerSemaphore -> IO a) -> IO a
withFreshSemaphore toks =
  bracket (freshSemaphore "sem_test" toks >>= unwrapServer)
    destroyServerSemaphore

-- | Assert that an action completes within the given number of
-- microseconds.
assertCompletes :: String -> Int -> IO a -> IO a
assertCompletes msg us action = do
  result <- timeout us action
  case result of
    Just a  -> return a
    Nothing -> do
      assertFailure (msg ++ ": timed out")
      error "unreachable"

-- | Assert that an action does NOT complete within 500 ms.
assertBlocks :: String -> IO a -> IO ()
assertBlocks msg action = do
  result <- timeout 500000 action
  case result of
    Nothing -> return ()
    Just _  -> assertFailure (msg ++ ": expected to block but completed")

-- | Wait (with timeout) for the semaphore pool to reach the expected value.
-- Polls every 50ms for up to 5 seconds.
awaitPoolValue :: ServerSemaphore -> Int -> IO ()
awaitPoolValue srv expected = go (100 :: Int)  -- 100 * 50ms = 5s
  where
    go 0 = do
      v <- getSemaphoreValue srv
      assertBool ("pool value: expected " ++ show expected ++ ", got " ++ show v) (v == expected)
    go n = do
      v <- getSemaphoreValue srv
      if v == expected
        then return ()
        else threadDelay 50000 >> go (n - 1)

-- | On Windows, Win32 named semaphores do not support crash recovery:
-- CloseHandle does not return tokens acquired via WaitForSingleObject.
-- Tests that expect crash recovery are marked as expected failures.
expectCrashRecoveryFail :: TestTree -> TestTree
#if defined(mingw32_HOST_OS)
expectCrashRecoveryFail = expectFailBecause
  "Win32 named semaphores do not support crash recovery"
#else
expectCrashRecoveryFail = id
#endif

--------------------------------------------------------------------------
-- Main

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "semaphore-compat"
  [ internalTests
  , versionCompatTests
  , createSemaphoreTests
  , freshSemaphoreTests
  , openSemaphoreTests
  , waitOnSemaphoreTests
  , tryWaitOnSemaphoreTests
  , releaseSemaphoreTests
  , getSemaphoreValueTests
  , destroySemaphoreTests
  , interruptibleWaitTests
  , semaphoreErrorTests
  , abstractSemTests
  , multiClientTests
  , sharedHandleTests
  , robustnessTests
  , subprocessTests
  , regressionTests
  ]

--------------------------------------------------------------------------
-- 1. Internal.Common (pure unit tests)

internalTests :: TestTree
internalTests = testGroup "Internal.Common"
  [ testGroup "parseSemaphoreIdentifier"
    [ testCase "valid: v2-foobar" $
        parseSemaphoreIdentifier "v2-foobar" @?= Just (SemaphoreName (v 2) "foobar")
    , testCase "valid: multi-digit version" $
        parseSemaphoreIdentifier "v12-bar" @?= Just (SemaphoreName (v 12) "bar")
    , testCase "invalid: empty" $
        parseSemaphoreIdentifier "" @?= Nothing
    , testCase "invalid: foo" $
        parseSemaphoreIdentifier "foo" @?= Nothing
    , testCase "invalid: v-foo (no digit)" $
        parseSemaphoreIdentifier "v-foo" @?= Nothing
    , testCase "invalid: v2foo (no dash)" $
        parseSemaphoreIdentifier "v2foo" @?= Nothing
    , testCase "invalid: 2-foo (no v)" $
        parseSemaphoreIdentifier "2-foo" @?= Nothing
    , testCase "invalid: vx-foo (non-numeric)" $
        parseSemaphoreIdentifier "vx-foo" @?= Nothing
    ]
  , testGroup "semaphoreIdentifier"
    [ testCase "v2 includes prefix" $
        semaphoreIdentifier (SemaphoreName (v 2) "foobar") @?= "v2-foobar"
    , testCase "v1 has no prefix" $
        semaphoreIdentifier (SemaphoreName (v 1) "foobar") @?= "foobar"
    , testCase "round-trips with parseSemaphoreIdentifier for v2" $
        fmap semaphoreIdentifier (parseSemaphoreIdentifier "v2-foobar")
          @?= Just "v2-foobar"
    ]
  , testGroup "iToBase62"
    [ testCase "0 -> \"0\"" $ Internal.iToBase62 0 @?= "0"
    , testCase "10 -> \"a\"" $ Internal.iToBase62 10 @?= "a"
    , testCase "36 -> \"A\"" $ Internal.iToBase62 36 @?= "A"
    , testCase "61 -> \"Z\"" $ Internal.iToBase62 61 @?= "Z"
    , testCase "62 -> \"10\"" $ Internal.iToBase62 62 @?= "10"
    , testCase "negative values produce valid output" $
        let result = Internal.iToBase62 (-42)
        in assertBool "should produce non-empty string" (not (null result))
    , testCase "output chars in valid range" $
        let valid  = ['0'..'9'] ++ ['a'..'z'] ++ ['A'..'Z']
            result = Internal.iToBase62 12345
        in assertBool "all chars should be base62" (all (`elem` valid) result)
    ]
  ]

--------------------------------------------------------------------------
-- 1b. Version compatibility

versionCompatTests :: TestTree
versionCompatTests = testGroup "versionsAreCompatible"
  [ testCase "same version is always compatible" $
      versionsAreCompatible (v 2) (v 2) @?= True
  , testCase "same version (v1)" $
      versionsAreCompatible (v 1) (v 1) @?= True
  , testCase "different versions incompatible" $
      versionsAreCompatible (v 1) (v 2) @?= False
  , testCase "different versions incompatible (reverse)" $
      versionsAreCompatible (v 2) (v 1) @?= False
  , testCase "openSemaphore: unversioned (v1) name" $ do
      result <- openSemaphore "no_prefix_compat"
#if defined(mingw32_HOST_OS)
      -- On Windows, semaphoreVersion = 1, so v1 names are compatible.
      -- openSemaphore tries to open (fails because semaphore doesn't exist).
      case result of
        Left (SemaphoreDoesNotExist _) -> return ()
        Left err -> assertFailure ("expected SemaphoreDoesNotExist, got " ++ show err)
        Right _ -> assertFailure "expected Left, got Right"
#else
      -- On POSIX, semaphoreVersion = 2, so v1 names are incompatible.
      case result of
        Left (SemaphoreIncompatibleVersion actual expected) -> do
          actual @?= v 1
          expected @?= semaphoreVersion
        Left err -> assertFailure ("expected SemaphoreIncompatibleVersion, got " ++ show err)
        Right _ -> assertFailure "expected Left, got Right"
#endif
  , testCase "openSemaphore: wrong version prefix" $ do
      result <- openSemaphore "v99-wrong_ver_compat"
      case result of
        Left (SemaphoreIncompatibleVersion actual expected) -> do
          actual @?= v 99
          expected @?= semaphoreVersion
        Left err -> assertFailure ("expected SemaphoreIncompatibleVersion, got " ++ show err)
        Right _ -> assertFailure "expected Left, got Right"
  ]

--------------------------------------------------------------------------
-- 2. createSemaphore

createSemaphoreTests :: TestTree
createSemaphoreTests = testGroup "createSemaphore"
  [ testCase "zero tokens" $ withSemaphore "create_zero" 0 $ \srv -> do
      v <- getSemaphoreValue srv
      v @?= 0
  , testCase "name structure" $ withSemaphore "create_struct" 1 $ \srv -> do
      let nameStr = semaphoreIdentifier (clientSemaphoreName (serverClientSemaphore srv))
          ver = getSemaphoreProtocolVersion semaphoreVersion
      if ver > 1
        then assertBool "should have version prefix" (("v" ++ show ver ++ "-") `isPrefixOf` nameStr)
        else assertBool "should be bare name" (head nameStr /= 'v' || '-' `notElem` take 4 nameStr)
  ]

--------------------------------------------------------------------------
-- 3. freshSemaphore

freshSemaphoreTests :: TestTree
freshSemaphoreTests = testGroup "freshSemaphore"
  [ testCase "two calls produce different names" $ do
      s1 <- freshSemaphore "fresh_a" 1 >>= unwrapServer
      s2 <- freshSemaphore "fresh_b" 1 >>= unwrapServer
      let n1 = semaphoreIdentifier (clientSemaphoreName (serverClientSemaphore s1))
          n2 = semaphoreIdentifier (clientSemaphoreName (serverClientSemaphore s2))
      assertBool "names should differ" (n1 /= n2)
      destroyServerSemaphore s1
      destroyServerSemaphore s2
  , testCase "name has version prefix" $ withFreshSemaphore 1 $ \srv -> do
      let nameStr = semaphoreIdentifier (clientSemaphoreName (serverClientSemaphore srv))
          ver = getSemaphoreProtocolVersion semaphoreVersion
      if ver > 1
        then assertBool "should have version prefix" (("v" ++ show ver ++ "-") `isPrefixOf` nameStr)
        else assertBool "should be bare name" (head nameStr /= 'v' || '-' `notElem` take 4 nameStr)
  ]

--------------------------------------------------------------------------
-- 4. openSemaphore

openSemaphoreTests :: TestTree
openSemaphoreTests = testGroup "openSemaphore"
  [ testCase "opens existing semaphore and shares state" $
      withSemaphore "open_share" 3 $ \srv -> do
        let name = semaphoreIdentifier (clientSemaphoreName (serverClientSemaphore srv))
        child <- openSemaphore (name) >>= unwrapSem
        -- child acquires a token; parent sees the change
        tok <- waitOnSemaphore child
        v <- getSemaphoreValue srv
        v @?= 2
        releaseSemaphoreToken tok
        destroyClientSemaphore child
  ]

--------------------------------------------------------------------------
-- 5. waitOnSemaphore

waitOnSemaphoreTests :: TestTree
waitOnSemaphoreTests = testGroup "waitOnSemaphore"
  [ testCase "basic: acquires token, value decrements" $
      withSemaphore "wait_basic" 3 $ \srv -> do
        let sem = serverClientSemaphore srv
        tok <- waitOnSemaphore sem
        v <- getSemaphoreValue srv
        v @?= 2
        releaseSemaphoreToken tok
  , testCase "blocks when exhausted, completes after release" $
      withSemaphore "wait_block" 1 $ \srv -> do
        let sem = serverClientSemaphore srv
            name = semaphoreIdentifier (clientSemaphoreName sem)
        tok <- waitOnSemaphore sem -- take the only token
        started <- newEmptyMVar
        done    <- newEmptyMVar
        _ <- forkIO $ do
          child <- openSemaphore (name) >>= unwrapSem
          childTok <- waitOnSemaphore child
          releaseSemaphoreToken childTok
          destroyClientSemaphore child
          putMVar done ()
        putMVar started ()
        takeMVar started
        threadDelay 100000 -- let thread reach the wait
        assertBlocks "wait should block when no tokens" (takeMVar done)
        releaseSemaphoreToken tok
        assertCompletes "wait should complete after release" 2000000 (takeMVar done)
  , testCase "multiple waiters all served" $
      withSemaphore "wait_multi" 3 $ \srv -> do
        let sem = serverClientSemaphore srv
            name = semaphoreIdentifier (clientSemaphoreName sem)
            numWaiters = 3
        -- Acquire all 3 tokens first, then fork waiters, then release.
        toks <- replicateM numWaiters (waitOnSemaphore sem)
        dones <- replicateM numWaiters newEmptyMVar
        forM_ dones $ \done -> forkIO $ do
          child <- openSemaphore (name) >>= unwrapSem
          childTok <- waitOnSemaphore child
          releaseSemaphoreToken childTok
          destroyClientSemaphore child
          putMVar done ()
        threadDelay 300000 -- let all waiters block
        mapM_ releaseSemaphoreToken toks
        forM_ dones $ \done ->
          assertCompletes "waiter should complete" 2000000 (takeMVar done)
  ]

--------------------------------------------------------------------------
-- 6. tryWaitOnSemaphore

tryWaitOnSemaphoreTests :: TestTree
tryWaitOnSemaphoreTests = testGroup "tryWaitOnSemaphore"
  [ testCase "returns Just when tokens available" $
      withSemaphore "try_true" 1 $ \srv -> do
        let sem = serverClientSemaphore srv
        r <- tryWaitOnSemaphore sem
        case r of
          Just tok -> releaseSemaphoreToken tok
          Nothing  -> assertFailure "expected Just, got Nothing"
  , testCase "non-blocking on empty semaphore" $
      withSemaphore "try_nonblock" 0 $ \srv -> do
        r <- assertCompletes "tryWait should not block" 1000000
               (tryWaitOnSemaphore (serverClientSemaphore srv))
        case r of
          Nothing -> return ()
          Just _  -> assertFailure "expected Nothing, got Just"
  , testCase "successive calls deplete tokens" $
      withSemaphore "try_deplete" 3 $ \srv -> do
        let sem = serverClientSemaphore srv
        r1 <- tryWaitOnSemaphore sem
        r2 <- tryWaitOnSemaphore sem
        r3 <- tryWaitOnSemaphore sem
        r4 <- tryWaitOnSemaphore sem
        -- r1..r3 should be Just, r4 should be Nothing
        case r1 of
          Just _  -> return ()
          Nothing -> assertFailure "r1: expected Just"
        case r2 of
          Just _  -> return ()
          Nothing -> assertFailure "r2: expected Just"
        case r3 of
          Just _  -> return ()
          Nothing -> assertFailure "r3: expected Just"
        case r4 of
          Nothing -> return ()
          Just _  -> assertFailure "r4: expected Nothing"
        -- Release acquired tokens
        mapM_ (\r -> case r of Just tok -> releaseSemaphoreToken tok; Nothing -> return ()) [r1, r2, r3]
  ]

--------------------------------------------------------------------------
-- 7. releaseSemaphoreToken

releaseSemaphoreTests :: TestTree
releaseSemaphoreTests = testGroup "releaseSemaphoreToken"
  [ testCase "increments value" $ withSemaphore "rel_inc" 1 $ \srv -> do
      let sem = serverClientSemaphore srv
      tok <- waitOnSemaphore sem
      releaseSemaphoreToken tok
      v <- getSemaphoreValue srv
      v @?= 1
  , testCase "release N tokens adds N tokens" $ withSemaphore "rel_n" 3 $ \srv -> do
      let sem = serverClientSemaphore srv
      toks <- replicateM 3 (waitOnSemaphore sem)
      mapM_ releaseSemaphoreToken toks
      v <- getSemaphoreValue srv
      v @?= 3
  ]

--------------------------------------------------------------------------
-- 8. getSemaphoreValue

getSemaphoreValueTests :: TestTree
getSemaphoreValueTests = testGroup "getSemaphoreValue"
  [ testCase "reports initial count" $ withSemaphore "getval_init" 5 $ \srv -> do
      v <- getSemaphoreValue srv
      v @?= 5
  ]

--------------------------------------------------------------------------
-- 9. destroySemaphore

destroySemaphoreTests :: TestTree
destroySemaphoreTests = testGroup "destroySemaphore"
  [ testCase "destroyed semaphore cannot be used" $ do
      srv <- freshSemaphore "destroy_open" 1 >>= unwrapServer
      let name = semaphoreIdentifier (clientSemaphoreName (serverClientSemaphore srv))
      destroyServerSemaphore srv
      -- openSemaphore must fail on a destroyed semaphore.
      result <- openSemaphore name
      case result of
        Left (SemaphoreDoesNotExist _) -> return ()
        Left  err -> assertFailure ("unexpected error from openSemaphore: " ++ show err)
        Right _   -> assertFailure "expected openSemaphore to fail on destroyed semaphore"
  , expectCrashRecoveryFail $
    testCase "client disconnect returns held tokens" $
      withSemaphore "destroy_client" 3 $ \srv -> do
        let name = semaphoreIdentifier (clientSemaphoreName (serverClientSemaphore srv))
        child <- openSemaphore (name) >>= unwrapSem
        tok1 <- waitOnSemaphore child
        tok2 <- waitOnSemaphore child
        v1 <- getSemaphoreValue srv
        v1 @?= 1
        -- Simulate crash: close the token fds without sending release.
        crashToken tok1
        crashToken tok2
        destroyClientSemaphore child
        awaitPoolValue srv 3
  ]

--------------------------------------------------------------------------
-- 10. SemaphoreError (failure cases)

semaphoreErrorTests :: TestTree
semaphoreErrorTests = testGroup "SemaphoreError"
  [ testCase "openSemaphore: wrong version returns error" $ do
      result <- openSemaphore "v1-err_wrongver"
      case result of
#if defined(mingw32_HOST_OS)
        -- On Windows, all versions are compatible, so it tries to open
        Left (SemaphoreDoesNotExist _) -> return ()
#else
        Left (SemaphoreIncompatibleVersion actual expected) -> do
          actual   @?= v 1
          expected @?= semaphoreVersion
#endif
        Left err -> assertFailure ("unexpected error: " ++ show err)
        Right _  -> assertFailure "expected Left, got Right"
  , testCase "openSemaphore: missing prefix returns version error on POSIX" $ do
      result <- openSemaphore "no_prefix_here"
      case result of
#if defined(mingw32_HOST_OS)
        -- On Windows, v1 is compatible, so it tries to open and fails
        Left (SemaphoreDoesNotExist _) -> return ()
#else
        Left (SemaphoreIncompatibleVersion actual expected) -> do
          actual @?= v 1
          expected @?= semaphoreVersion
#endif
        Left err -> assertFailure ("unexpected error: " ++ show err)
        Right _  -> assertFailure "expected Left, got Right"
  , testCase "openSemaphore: nonexistent semaphore fails" $ do
      let ident = semaphoreIdentifier (SemaphoreName semaphoreVersion "err_noexist")
      result <- openSemaphore ident
      case result of
        Left (SemaphoreDoesNotExist _) -> return ()
        Left  err -> assertFailure ("unexpected error: " ++ show err)
        Right _   -> assertFailure "expected openSemaphore to fail on nonexistent semaphore"
  , testCase "openSemaphore: empty string returns version error on POSIX" $ do
      result <- openSemaphore ""
      case result of
#if defined(mingw32_HOST_OS)
        -- On Windows, v1 is compatible, so it tries to open the empty name.
        -- Win32 OpenSemaphore() rejects the empty name with ERROR_INVALID_HANDLE
        -- (not ERROR_FILE_NOT_FOUND), so we get SemaphoreOtherError here.
        Left (SemaphoreOtherError _) -> return ()
#else
        Left (SemaphoreIncompatibleVersion actual expected) -> do
          actual @?= v 1
          expected @?= semaphoreVersion
#endif
        Left err -> assertFailure ("unexpected error: " ++ show err)
        Right _  -> assertFailure "expected Left, got Right"
  ]

--------------------------------------------------------------------------
-- 11. Interruptible waitOnSemaphore

interruptibleWaitTests :: TestTree
interruptibleWaitTests = testGroup "interruptible waitOnSemaphore"
  [ testCase "succeeds when token available" $
      withSemaphore "int_ok" 1 $ \srv -> do
        let sem = serverClientSemaphore srv
        resultVar <- newEmptyMVar
        _ <- forkIO $ do
          r <- try (waitOnSemaphore sem) :: IO (Either SomeException SemaphoreToken)
          putMVar resultVar r
        result <- assertCompletes "should get token" 2000000
                    (takeMVar resultVar)
        case result of
          Right tok -> releaseSemaphoreToken tok
          Left e    -> assertFailure ("expected Right token, got Left " ++ show e)
  , testCase "can be cancelled on 0-token semaphore" $
      withSemaphore "int_cancel" 0 $ \srv -> do
        let sem = serverClientSemaphore srv
        resultVar <- newEmptyMVar
        tid <- forkIO $ do
          r <- try (waitOnSemaphore sem) :: IO (Either SomeException SemaphoreToken)
          putMVar resultVar r
        threadDelay 100000 -- let it block
        assertCompletes "killThread should not hang" 2000000
          (killThread tid)
        result <- assertCompletes "should get result" 2000000
                    (takeMVar resultVar)
        case result of
          Left _  -> return ()
          Right _ -> assertFailure "expected Left (interrupted), got Right"
  , testCase "cancelled wait does not leak tokens" $
      withSemaphore "int_noleak" 1 $ \srv -> do
        let sem = serverClientSemaphore srv
        -- take the only token
        tok <- waitOnSemaphore sem
        v0 <- getSemaphoreValue srv
        v0 @?= 0
        -- fork a wait that will block (no tokens available)
        tid <- forkIO $ void (waitOnSemaphore sem)
        threadDelay 100000
        -- cancel the wait
        assertCompletes "killThread should not hang" 2000000
          (killThread tid)
        -- release our token
        releaseSemaphoreToken tok
        -- The server should detect the cancelled wait's disconnect
        -- and return any transiently acquired token.
        tok' <- assertCompletes "token should be available" 5000000 $
          waitOnSemaphore sem
        v1 <- getSemaphoreValue srv
        v1 @?= 0
        releaseSemaphoreToken tok'
  ]

--------------------------------------------------------------------------
-- 12. AbstractSem

abstractSemTests :: TestTree
abstractSemTests = testGroup "AbstractSem"
  [ testCase "withAbstractSem acquires and releases" $
      withSemaphore "absem_basic" 1 $ \srv -> do
        let sem = serverClientSemaphore srv
        tokRef <- newIORef (undefined :: SemaphoreToken)
        let absSem = AbstractSem
              (waitOnSemaphore sem >>= writeIORef tokRef)
              (readIORef tokRef >>= releaseSemaphoreToken)
        v0 <- getSemaphoreValue srv
        v0 @?= 1
        withAbstractSem absSem $ do
          v1 <- getSemaphoreValue srv
          v1 @?= 0
        v2 <- getSemaphoreValue srv
        v2 @?= 1
  , testCase "releases on exception (bracket safety)" $
      withSemaphore "absem_exc" 1 $ \srv -> do
        let sem = serverClientSemaphore srv
        tokRef <- newIORef (undefined :: SemaphoreToken)
        let absSem = AbstractSem
              (waitOnSemaphore sem >>= writeIORef tokRef)
              (readIORef tokRef >>= releaseSemaphoreToken)
        result <- try $ withAbstractSem absSem $
          error "boom"
        case result of
          Left (_ :: SomeException) -> return ()
          Right (_ :: ()) -> assertFailure "expected exception"
        v <- getSemaphoreValue srv
        v @?= 1
  ]

--------------------------------------------------------------------------
-- 12. Multi-client (freshSemaphore, concurrency limiting)

multiClientTests :: TestTree
multiClientTests = testGroup "multi-client"
  [ testCase "freshSemaphore: children open by name and respect limit" $
      withFreshSemaphore 3 $ \srv -> do
        let nameStr = semaphoreIdentifier (clientSemaphoreName (serverClientSemaphore srv))
        ref <- newIORef (0 :: Int, 0 :: Int)
        let numChildren = 6
        dones <- replicateM numChildren newEmptyMVar
        forM_ (zip [1::Int ..] dones) $ \(_n, done) -> forkIO $ do
          child <- openSemaphore (nameStr) >>= unwrapSem
          tok <- waitOnSemaphore child
          atomicModifyIORef' ref
            (\(cur, peak) -> let cur' = cur + 1 in ((cur', max peak cur'), ()))
          threadDelay 80000 -- 80 ms of work
          atomicModifyIORef' ref (\(cur, peak) -> ((cur - 1, peak), ()))
          releaseSemaphoreToken tok
          destroyClientSemaphore child
          putMVar done ()
        mapM_ takeMVar dones
        (_, peak) <- readIORef ref
        assertBool ("peak " ++ show peak ++ " should be <= 3") (peak <= 3)
        val <- getSemaphoreValue srv
        val @?= 3
  ]

--------------------------------------------------------------------------
-- 14. Shared-handle (multithreaded use of a single Semaphore value)

sharedHandleTests :: TestTree
sharedHandleTests = testGroup "shared handle"
  [ testCase "concurrent blocking wait + tryWait on same handle" $
      withFreshSemaphore 1 $ \srv -> do
        let name = semaphoreIdentifier (clientSemaphoreName (serverClientSemaphore srv))
        forM_ [1..50 :: Int] $ \_ -> do
          -- Use a helper connection to drain the token so pool = 0.
          helper <- openSemaphore name >>= unwrapSem
          helperTok <- waitOnSemaphore helper
          -- Shared client identity used by two threads concurrently.
          client <- openSemaphore name >>= unwrapSem
          -- Thread 1: blocking wait (opens fresh connection, blocks)
          t1done <- newEmptyMVar
          _ <- forkIO $ do
            r <- try (waitOnSemaphore client) :: IO (Either SomeException SemaphoreToken)
            putMVar t1done r
          threadDelay 50000 -- let thread 1 block
          -- Main thread: tryWait on same identity (opens fresh connection).
          t2done <- newEmptyMVar
          _ <- forkIO $ do
            r <- try (tryWaitOnSemaphore client) :: IO (Either SomeException (Maybe SemaphoreToken))
            putMVar t2done r
          r2 <- timeout 5000000 (takeMVar t2done)
          releaseSemaphoreToken helperTok
          r1 <- timeout 5000000 (takeMVar t1done)
          -- Check that thread 1 succeeded.
          case r1 of
            Just (Right tok) -> releaseSemaphoreToken tok
            Just (Left e) -> assertFailure ("waitOnSemaphore failed: " ++ show e)
            Nothing -> assertFailure "thread 1 deadlocked"
          -- Check that tryWait returned Nothing (no tokens available at time of try).
          case r2 of
            Just (Right Nothing)    -> return ()
            Just (Right (Just tok)) -> releaseSemaphoreToken tok  -- got lucky, that's ok too
            Just (Left e)           -> assertFailure ("tryWait exception: " ++ show e)
            Nothing                 -> assertFailure "tryWait deadlocked"
          destroyClientSemaphore client
          destroyClientSemaphore helper
  , testCase "release from one connection unblocks wait on another" $
      withFreshSemaphore 1 $ \srv -> do
        let name = semaphoreIdentifier (clientSemaphoreName (serverClientSemaphore srv))
        -- Connection A acquires the only token.
        connA <- openSemaphore name >>= unwrapSem
        tokA <- waitOnSemaphore connA
        -- Connection B waits (will block -- pool is 0).
        connB <- openSemaphore name >>= unwrapSem
        done <- newEmptyMVar
        _ <- forkIO $ do
          tokB <- waitOnSemaphore connB
          putMVar done tokB
        threadDelay 100000 -- let thread reach the wait
        -- Connection A releases; connection B should be unblocked.
        releaseSemaphoreToken tokA
        tokB <- assertCompletes "connB should complete after release" 5000000
          (takeMVar done)
        -- connB now holds the token; release it.
        releaseSemaphoreToken tokB
        v <- getSemaphoreValue srv
        v @?= 1
        destroyClientSemaphore connA
        destroyClientSemaphore connB
  , testCase "Cabal-like worker pattern: wait then fork release" $
      withFreshSemaphore 2 $ \srv -> do
        let sem = serverClientSemaphore srv
            numJobs = 6
        dones <- replicateM numJobs newEmptyMVar
        -- Spawn workers: each acquires a token, forks work that releases it.
        forM_ (zip [1::Int ..] dones) $ \(_n, done) -> forkIO $ do
          tok <- waitOnSemaphore sem
          _ <- forkIO $ do
            threadDelay 50000 -- simulate work
            releaseSemaphoreToken tok
            putMVar done ()
          return ()
        forM_ dones $ \done ->
          assertCompletes "job should complete" 10000000 (takeMVar done)
        v <- getSemaphoreValue srv
        v @?= 2
  , expectCrashRecoveryFail $
    testCase "disconnect without release returns tokens, waiters on other connections unblocked" $
      withFreshSemaphore 2 $ \srv -> do
        let name = semaphoreIdentifier (clientSemaphoreName (serverClientSemaphore srv))
        -- Client A acquires both tokens (each waitOnSemaphore opens a fresh connection)
        clientA <- openSemaphore (name) >>= unwrapSem
        tokA1 <- waitOnSemaphore clientA
        tokA2 <- waitOnSemaphore clientA
        v0 <- getSemaphoreValue srv
        v0 @?= 0
        -- Client B waits on a separate connection (will block)
        clientB <- openSemaphore (name) >>= unwrapSem
        done <- newEmptyMVar
        _ <- forkIO $ do
          tokB <- waitOnSemaphore clientB
          putMVar done tokB
        threadDelay 200000 -- let B block
        -- Simulate crash: close the token fds without sending release.
        crashToken tokA1
        crashToken tokA2
        destroyClientSemaphore clientA
        -- Client B should be unblocked by the returned tokens.
        tokB <- assertCompletes "client B should complete after A crashes" 5000000
          (takeMVar done)
        releaseSemaphoreToken tokB
        destroyClientSemaphore clientB
  ]

--------------------------------------------------------------------------
-- 15. Robustness (crash recovery, async exceptions, masking)

robustnessTests :: TestTree
robustnessTests = testGroup "robustness"
  [ testGroup "crash recovery"
    [ expectCrashRecoveryFail $
      testCase "multiple clients crash, all tokens recovered" $
        withSemaphore "robust_multi_crash" 5 $ \srv -> do
          let name = semaphoreIdentifier (clientSemaphoreName (serverClientSemaphore srv))
          -- Each child opens, acquires a token, and crashes (fd closed
          -- without sending release).
          replicateM_ 5 $ do
            child <- openSemaphore (name) >>= unwrapSem
            tok <- waitOnSemaphore child
            crashToken tok
            destroyClientSemaphore child
          awaitPoolValue srv 5
    , expectCrashRecoveryFail $
      testCase "client holding many tokens crashes" $
        withSemaphore "robust_many_toks" 10 $ \srv -> do
          let name = semaphoreIdentifier (clientSemaphoreName (serverClientSemaphore srv))
          child <- openSemaphore (name) >>= unwrapSem
          toks <- replicateM 10 (waitOnSemaphore child)
          v0 <- getSemaphoreValue srv
          v0 @?= 0
          mapM_ crashToken toks
          destroyClientSemaphore child
          awaitPoolValue srv 10
    ]
  , testGroup "async exceptions"
    [ expectCrashRecoveryFail $
      testCase "throwTo kills thread holding token, no leak" $
        withFreshSemaphore 1 $ \srv -> do
          let name = semaphoreIdentifier (clientSemaphoreName (serverClientSemaphore srv))
          child <- openSemaphore (name) >>= unwrapSem
          tidVar <- newEmptyMVar
          tokRef <- newIORef Nothing
          _ <- forkIO $ do
            tid <- myThreadId
            putMVar tidVar tid
            tok <- waitOnSemaphore child -- acquires the token
            writeIORef tokRef (Just tok)
            threadDelay 10000000  -- long sleep; throwTo lands here (unmasked)
          tid <- takeMVar tidVar
          threadDelay 200000 -- let thread acquire and enter sleep
          v0 <- getSemaphoreValue srv
          v0 @?= 0
          throwTo tid ThreadKilled
          -- Thread is dead; simulate crash by closing the token fd.
          mbTok <- readIORef tokRef
          case mbTok of
            Just tok -> crashToken tok
            Nothing  -> return ()
          destroyClientSemaphore child
          awaitPoolValue srv 1
#if !defined(mingw32_HOST_OS)
      -- On Win32, crashToken is a no-op (no crash recovery), so when
      -- throwTo lands between mask_ blocks, the unreleased tokens are
      -- permanently lost.  This makes the test flaky — skip it.
    , testCase "throwTo during releaseSemaphoreToken (masked)" $
        withSemaphore "robust_throwto_rel" 5 $ \srv -> do
          let sem = serverClientSemaphore srv
          toks <- replicateM 5 (waitOnSemaphore sem)
          v0 <- getSemaphoreValue srv
          v0 @?= 0
          -- Track which tokens have been released.
          remainingRef <- newIORef toks
          tidVar <- newEmptyMVar
          _ <- forkIO $ do
            tid <- myThreadId
            putMVar tidVar tid
            let releaseAll [] = return ()
                releaseAll (t:ts) = do
                  -- mask_ ensures the IORef update is atomic with the
                  -- release, so remainingRef always accurately reflects
                  -- which tokens are still open (prevents double-close
                  -- of recycled fd numbers).
                  mask_ $ do
                    releaseSemaphoreToken t
                    writeIORef remainingRef ts
                  releaseAll ts
            releaseAll toks
          tid <- takeMVar tidVar
          throwTo tid ThreadKilled
          -- Crash-close any tokens the thread didn't get to release.
          remaining <- readIORef remainingRef
          mapM_ crashToken remaining
          -- Either mask_ protected the release, or crash recovery
          -- returns unreleased tokens; either way all 5 are restored.
          awaitPoolValue srv 5
#endif
    ]
  , testGroup "interruptible wait edge cases"
    [ testCase "double interrupt is safe" $
        withSemaphore "robust_dbl_int" 0 $ \srv -> do
          let sem = serverClientSemaphore srv
          doneVar <- newEmptyMVar
          tid <- forkIO $ do
            void (try (waitOnSemaphore sem) :: IO (Either SomeException SemaphoreToken))
            putMVar doneVar ()
          threadDelay 100000
          killThread tid
          void $ assertCompletes "thread should be dead" 2000000
                   (takeMVar doneVar)
          -- second killThread: thread is dead, should not hang
          _ <- assertCompletes "second killThread should not hang" 2000000
                 (try (killThread tid) :: IO (Either SomeException ()))
          return ()
    , testCase "killThread after natural completion" $
        withSemaphore "robust_int_done" 1 $ \srv -> do
          let sem = serverClientSemaphore srv
          resultVar <- newEmptyMVar
          tid <- forkIO $ do
            r <- try (waitOnSemaphore sem) :: IO (Either SomeException SemaphoreToken)
            putMVar resultVar r
          result <- assertCompletes "should complete" 2000000
                      (takeMVar resultVar)
          case result of
            Right tok -> releaseSemaphoreToken tok
            Left e    -> assertFailure ("expected Right token, got Left " ++ show e)
          threadDelay 100000
          -- killThread after completion: should not crash
          _ <- try (killThread tid) :: IO (Either SomeException ())
          return ()
    , testCase "many cancelled waits don't leak" $
        withSemaphore "robust_cancel_many" 1 $ \srv -> do
          let sem = serverClientSemaphore srv
          -- Acquire the token so waits will block.
          tok <- waitOnSemaphore sem
          tids <- replicateM 10 $
            forkIO $ void (waitOnSemaphore sem)
          threadDelay 200000
          forM_ tids $ \tid ->
            void (try (killThread tid) :: IO (Either SomeException ()))
          -- Wait for all cancelled waits to be cleaned up, then release.
          awaitPoolValue srv 0
          releaseSemaphoreToken tok
          awaitPoolValue srv 1
    , testCase "cancel-vs-complete race: no token leak" $
        withFreshSemaphore 1 $ \srv -> do
          let sem = serverClientSemaphore srv
              name = semaphoreIdentifier (clientSemaphoreName (serverClientSemaphore srv))
          forM_ [1..10 :: Int] $ \_ -> do
            conn <- openSemaphore name >>= unwrapSem
            connTok <- waitOnSemaphore conn
            resultVar <- newEmptyMVar
            tid <- forkIO $ mask $ \unmask -> do
              r <- try (unmask $ waitOnSemaphore conn) :: IO (Either SomeException SemaphoreToken)
              putMVar resultVar r
            threadDelay 50000 -- let thread block
            -- Race: release and cancel concurrently
            releaseDone <- newEmptyMVar
            _ <- forkIO $ releaseSemaphoreToken connTok >> putMVar releaseDone ()
            _ <- try (killThread tid) :: IO (Either SomeException ())
            result <- assertCompletes "should get result" 2000000
                     (takeMVar resultVar)
            -- If the thread got a token, release it
            case result of
              Right tok -> releaseSemaphoreToken tok
              Left _    -> return ()
            -- Wait for the forked release to complete before destroying
            -- the connection.  Without this, destroyClientSemaphore can
            -- close the semaphore handle while releaseSemaphore is still
            -- running on another thread, losing the token.
            takeMVar releaseDone
            awaitPoolValue srv 1
            destroyClientSemaphore conn
    ]
  , testGroup "concurrent stress"
    [ testCase "rapid create-destroy cycle" $
        forM_ [1..50 :: Int] $ \_ -> do
          srv <- freshSemaphore "robust_rapid" 1 >>= unwrapServer
          destroyServerSemaphore srv
    , testCase "destroyServerSemaphore is idempotent" $ do
        srv <- freshSemaphore "robust_idempotent_destroy" 1 >>= unwrapServer
        destroyServerSemaphore srv
        -- Second call is a no-op.
        assertCompletes "second destroyServerSemaphore should not hang" 2000000
          (destroyServerSemaphore srv)
    ]
  ]

--------------------------------------------------------------------------
-- 15. Subprocess robustness (real process death via helper executable)

subprocessTests :: TestTree
subprocessTests = testGroup "subprocess robustness"
  [ testGroup "crash recovery"
    [ expectCrashRecoveryFail $
      testCase "multiple processes crash, all tokens recovered" $
        withSemaphore "sub_multi_crash" 5 $ \srv -> do
          let name = semaphoreIdentifier (clientSemaphoreName (serverClientSemaphore srv))
          -- Spawn 5 helpers, each opens and acquires one token
          replicateM_ 5 $ withHelper $ \h -> do
            sendCommand_ h (CmdOpen name)
            sendCommand_ h CmdWait
            sendCommandNoResponse h CmdCrash
            _ <- try (waitForProcess (hProcess h)) :: IO (Either SomeException ExitCode)
            return ()
          -- Wait for server to detect disconnects
          threadDelay 1000000
          v1 <- getSemaphoreValue srv
          v1 @?= 5
    , expectCrashRecoveryFail $
      testCase "process holding many tokens crashes" $
        withSemaphore "sub_many_toks" 10 $ \srv -> do
          let name = semaphoreIdentifier (clientSemaphoreName (serverClientSemaphore srv))
          withHelper $ \h -> do
            sendCommand_ h (CmdOpen name)
            sendCommand_ h (CmdWaitN 10)
            v0 <- getSemaphoreValue srv
            v0 @?= 0
            sendCommandNoResponse h CmdCrash
            _ <- try (waitForProcess (hProcess h)) :: IO (Either SomeException ExitCode)
            return ()
          threadDelay 1000000
          v1 <- getSemaphoreValue srv
          v1 @?= 10
    ]
  , testGroup "process kill"
    [ expectCrashRecoveryFail $
      testCase "process killed while holding token" $
        withSemaphore "sub_kill_hold" 1 $ \srv -> do
          let name = semaphoreIdentifier (clientSemaphoreName (serverClientSemaphore srv))
          withHelper $ \h -> do
            sendCommand_ h (CmdOpen name)
            sendCommand_ h CmdWait
            v0 <- getSemaphoreValue srv
            v0 @?= 0
            killHelper h
          threadDelay 1000000
          v1 <- getSemaphoreValue srv
          v1 @?= 1
    , testCase "process killed during blocked wait, no leak" $
        withSemaphore "sub_kill_block" 0 $ \srv -> do
          let name = semaphoreIdentifier (clientSemaphoreName (serverClientSemaphore srv))
          withHelper $ \h -> do
            sendCommand_ h (CmdOpen name)
            -- Send wait but don't read response (it will block)
            sendCommandNoResponse h CmdWait
            threadDelay 200000 -- let helper block
            killHelper h
          threadDelay 1000000
          v <- getSemaphoreValue srv
          v @?= 0
    ]
  , testGroup "multi-process edge cases"
    [ expectCrashRecoveryFail $
      testCase "kill-vs-complete race" $
        withSemaphore "sub_kill_race" 1 $ \srv -> do
          let sem = serverClientSemaphore srv
              name = semaphoreIdentifier (clientSemaphoreName sem)
          -- Acquire the token so the helper's wait will block.
          tok <- waitOnSemaphore sem
          withHelper $ \h -> do
            sendCommand_ h (CmdOpen name)
            sendCommandNoResponse h CmdWait
            threadDelay 200000
            -- Release a token so the helper can acquire it, then
            -- wait until the helper has acquired before killing.
            releaseSemaphoreToken tok
            -- Spin until the helper has consumed the token.
            let waitUntilAcquired = do
                  v <- getSemaphoreValue srv
                  if v == 0 then return ()
                  else threadDelay 10000 >> waitUntilAcquired
            assertCompletes "helper should acquire" 5000000 waitUntilAcquired
            killHelper h
          threadDelay 1000000
          v <- getSemaphoreValue srv
          -- The helper acquired the token and was killed.
          -- On Posix, crash recovery returns the token: pool = 1.
          -- On Win32, the token is lost: pool = 0.
          v @?= 1
    ]
  , testGroup "concurrent waiter crash interaction"
    [ expectCrashRecoveryFail $
      testCase "process crash returns tokens, unblocking waiting process" $
        withSemaphore "sub_crash_unblock" 2 $ \srv -> do
          let name = semaphoreIdentifier (clientSemaphoreName (serverClientSemaphore srv))
          -- Use withHelper to ensure cleanup on test failure (expected on Windows)
          withHelper $ \hA -> withHelper $ \hB -> do
            -- Process A acquires both tokens
            sendCommand_ hA (CmdOpen name)
            sendCommand_ hA (CmdWaitN 2)
            v0 <- getSemaphoreValue srv
            v0 @?= 0
            -- Process B sends a blocking wait
            sendCommand_ hB (CmdOpen name)
            sendCommandNoResponse hB CmdWait
            threadDelay 200000 -- let B's wait reach the server
            -- Crash process A: tokens returned to pool, B should unblock
            sendCommandNoResponse hA CmdCrash
            _ <- try (waitForProcess (hProcess hA)) :: IO (Either SomeException ExitCode)
            -- Wait until B has acquired the token (pool goes from 2 to 1)
            let waitUntilAcquired = do
                  v <- getSemaphoreValue srv
                  if v == 1 then return ()
                  else threadDelay 10000 >> waitUntilAcquired
            assertCompletes "process B should acquire after A crashes" 5000000
              waitUntilAcquired
            killHelper hB
            threadDelay 1000000
            v <- getSemaphoreValue srv
            v @?= 2
    , testCase "process killed with pending wait, no token leak" $
        withSemaphore "sub_kill_pending" 0 $ \srv -> do
          let name = semaphoreIdentifier (clientSemaphoreName (serverClientSemaphore srv))
          -- Process sends a wait that will block (0 tokens)
          withHelper $ \hA -> do
            sendCommand_ hA (CmdOpen name)
            sendCommandNoResponse hA CmdWait
            threadDelay 200000 -- let wait reach the server
            -- Helper is blocked in waitOnSemaphore so it can't read
            -- CmdCrash from stdin; use killHelper (SIGKILL) instead.
            killHelper hA
          threadDelay 1000000
          -- No tokens were held, pending wait discarded, pool still 0
          v <- getSemaphoreValue srv
          v @?= 0
    , expectCrashRecoveryFail $
      testCase "process killed with held tokens and pending wait" $
        withSemaphore "sub_kill_held_pending" 1 $ \srv -> do
          let name = semaphoreIdentifier (clientSemaphoreName (serverClientSemaphore srv))
          -- Process acquires the token, then sends another wait (will block)
          withHelper $ \hA -> do
            sendCommand_ hA (CmdOpen name)
            sendCommand_ hA CmdWait   -- acquires (pool=0)
            sendCommandNoResponse hA CmdWait  -- pending (pool=0)
            threadDelay 200000 -- let pending wait register
            v0 <- getSemaphoreValue srv
            v0 @?= 0
            -- Helper is blocked in waitOnSemaphore; use killHelper.
            -- 1 held token returned, pending wait discarded.
            killHelper hA
          threadDelay 1000000
          v <- getSemaphoreValue srv
          v @?= 1
    ]
  , testGroup "cross-process concurrency"
    [ testCase "cross-process concurrency limiting" $
        withSemaphore "sub_conc_limit" 4 $ \srv -> do
          let name = semaphoreIdentifier (clientSemaphoreName (serverClientSemaphore srv))
          dones <- replicateM 10 $ do
            done <- newEmptyMVar
            _ <- forkIO $ withHelper $ \h -> do
              sendCommand_ h (CmdOpen name)
              sendCommand_ h CmdWait
              sendCommand_ h (CmdRelease 1)
              sendCommand_ h CmdDestroy
              putMVar done ()
            return done
          forM_ dones $ \done ->
            assertCompletes "helper should complete" 10000000 (takeMVar done)
          v <- getSemaphoreValue srv
          v @?= 4
    ]
  ]

--------------------------------------------------------------------------
-- Regression tests.

regressionTests :: TestTree
regressionTests = testGroup "regression"
  [
#if !defined(mingw32_HOST_OS)
  -- Path length check: paths longer than sun_path must be rejected,
  -- not silently truncated.
    testCase "path too long is rejected" $ do
      let longName = replicate 200 'x'
      result <- createSemaphore longName 1
      case result of
        Left _  -> return ()
        Right s -> do destroyServerSemaphore s
                      assertFailure "expected failure for path exceeding sun_path"

  -- Release guard: a CmdRelease on a connection that hasn't acquired
  -- any tokens must not increment the pool past its initial value.
  , testCase "release without acquire does not corrupt pool" $
      withFreshSemaphore 2 $ \srv -> do
        let path = Internal.getSemaphoreSocketPath
                     (clientSemaphoreName $ serverClientSemaphore srv)
        socketPath <- path
        -- Open a raw connection and send CmdRelease without CmdWait.
        fd <- connectDomainSocket socketPath
        fdWriteByte fd 0x2B  -- '+'
        resp <- fdReadByte fd
        closeFd fd
        -- Server should reject: response is '!' (RspFail)
        resp @?= 0x21
        -- Pool must still be 2, not 3.
        v <- getSemaphoreValue srv
        v @?= 2

  -- destroyServerSemaphore must not hang when serve threads are
  -- blocked in read.
  , testCase "destroyServerSemaphore does not hang with active connections" $ do
      srv <- freshSemaphore "reg_hang" 1 >>= unwrapServer
      -- Acquire a token but do NOT release it.  The serve thread is
      -- now blocked in fdReadByte waiting for the next command on
      -- this connection.
      _tok <- waitOnSemaphore (serverClientSemaphore srv)
      -- destroyServerSemaphore must kill this blocked serve thread.
      result <- timeout 5000000 $ destroyServerSemaphore srv
      case result of
        Just _  -> return ()
        Nothing -> assertFailure
          "destroyServerSemaphore hung (serve thread not interruptible)"

  -- Rapid create/acquire/destroy must not corrupt fds (e.g. orphaned
  -- serve threads closing recycled fds).
  , testCase "rapid create-acquire-destroy does not corrupt fds" $
      forM_ [1..100 :: Int] $ \_ -> do
        srv <- freshSemaphore "reg_rapid" 1 >>= unwrapServer
        tok <- waitOnSemaphore (serverClientSemaphore srv)
        releaseSemaphoreToken tok
        destroyServerSemaphore srv

  -- Crash after acquire must return the token to the pool.
  , testCase "crash after acquire returns token to pool" $
      withFreshSemaphore 1 $ \srv -> do
        let sem = serverClientSemaphore srv
        let path = Internal.getSemaphoreSocketPath (clientSemaphoreName sem)
        socketPath <- path
        -- Open a raw connection and acquire one token.
        fd <- connectDomainSocket socketPath
        fdWriteByte fd 0x2D  -- '-' = CmdWait
        resp <- fdReadByte fd  -- should get '.' = RspOk
        resp @?= 0x2E
        -- Pool should now be 0.
        v0 <- getSemaphoreValue srv
        v0 @?= 0
        -- Crash: close the fd without sending release.
        closeFd fd
        -- Server should detect disconnect and return the token.
        awaitPoolValue srv 1

  -- Cancel a blocked wait then immediately acquire.  Exercises the
  -- shutdown-based cancellation under rapid repeated use to ensure
  -- no fd corruption or token leak.
  , testCase "cancel wait then immediately acquire" $
      withFreshSemaphore 1 $ \srv -> do
        let sem = serverClientSemaphore srv
        -- Hold the token so waits block.
        tok <- waitOnSemaphore sem
        forM_ [1..50 :: Int] $ \_ -> do
          tid <- forkIO $ void (waitOnSemaphore sem)
          threadDelay 10000
          killThread tid
        -- The original token should still work (its fd wasn't corrupted).
        releaseSemaphoreToken tok
        awaitPoolValue srv 1

  -- Token-per-connection: each waitOnSemaphore opens its own connection,
  -- so concurrent waits must not interfere.
  , testCase "concurrent waits get independent connections" $
      withFreshSemaphore 10 $ \srv -> do
        let sem = serverClientSemaphore srv
        doneVars <- replicateM 10 $ do
          done <- newEmptyMVar
          _ <- forkIO $ do
            tok <- waitOnSemaphore sem
            releaseSemaphoreToken tok
            putMVar done ()
          return done
        forM_ doneVars $ \done ->
          assertCompletes "concurrent wait should complete" 5000000
            (takeMVar done)
        awaitPoolValue srv 10

  -- A SemaphoreToken that becomes unreachable without being released
  -- must eventually return its token to the pool via the MVar finalizer.
  , testCase "GC finalizer returns dropped token to pool" $
      withFreshSemaphore 1 $ \srv -> do
        let sem = serverClientSemaphore srv
        -- Acquire a token in a scope where the binding is immediately lost.
        void $ waitOnSemaphore sem
        performMajorGC
        awaitPoolValue srv 1
#endif
  ]
