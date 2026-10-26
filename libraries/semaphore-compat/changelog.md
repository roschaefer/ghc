### 2.0.1 (June 30, 2026)

- Fix a bug on Darwin where accepted fds would inherit O_NONBLOCK from the listen fd,
  causing subsequent reads on the server to fail instantly.
- Fix fds opened by `semaphore-compat` leaking across `exec`.
- Avoid leaking fds from stderr handles in the testsuite.
- Improve documentation
- Drop dependency on `exceptions`
- `releaseSemaphoreToken` is now idempotent on Windows.

### 2.0.0 (May 22, 2026)

- Major release introducing protocol v2:
    - On Linux and other POSIX platforms, the implementation now uses
      Unix domain sockets in place of POSIX named semaphores
      (`sem_open(3)`), avoiding the libc-ABI issues that affected the
      v1 implementation when a jobserver and its clients were built
      against different C standard libraries (see
      [GHC #25087](https://gitlab.haskell.org/ghc/ghc/-/issues/25087)
      and [cabal #9993](https://github.com/haskell/cabal/issues/9993)).
    - Windows is unaffected and continues to use Win32 named
      semaphores; its reported protocol version remains v1.
- API overhaul:
    - `Semaphore` has been renamed to `ClientSemaphore`.
    - `ServerSemaphore` is a new type representing the server-side
      identity of a semaphore, with full bookkeeping for crash
      recovery on POSIX.  Use `serverClientSemaphore` to recover the
      `ClientSemaphore` from a `ServerSemaphore`.
    - `SemaphoreToken` is a new type representing a single held token;
      `waitOnSemaphore` returns one and `releaseSemaphoreToken` (or
      `withSemaphoreToken`) releases one.
    - `SemaphoreName` is now a record carrying the protocol version
      alongside the unversioned name; use `semaphoreIdentifier` to derive
      the serialised name for transport between processes via flags like
      `-jsem`.
- `versionsAreCompatible :: SemaphoreProtocolVersion ->
  SemaphoreProtocolVersion -> Bool` exposed for use by jobservers
  (e.g. cabal-install) to detect protocol mismatches before invoking a
  jobclient (e.g. GHC).
- `waitOnSemaphore` is now interruptible: an asynchronous exception
  delivered while a thread is blocked on the semaphore aborts the wait
  safely.
- POSIX server-side crash recovery: when a client disconnects without
  explicitly returning its tokens, the server returns them to the pool
  automatically.  This is not available on Windows.

### 1.0.0 (March 13, 2023)

- First version of the `semaphore-compat` package.
