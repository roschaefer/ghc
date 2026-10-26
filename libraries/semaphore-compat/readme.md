# semaphore-compat

`semaphore-compat` provides cross-platform semaphores that can be shared across processes.

It uses different backends on different platforms:

  - POSIX: Unix domain sockets with automatic return of tokens from
    disconnected clients.
  - Windows: Win32 named semaphores. Tokens held by crashed clients are not
    returned automatically.
  - wasm/JavaScript: buildable stubs that throw unsupported-operation errors.
    Avoid calling this code on these backends.

It supports:

  - Creating server semaphores (`createSemaphore`, `freshSemaphore`).
    - A server is the process that creates the semaphore and must stay alive for
      the duration of its use.
    - A server process can also act as a client and request/release tokens from the
      semaphore.
  - Opening client semaphores (`openSemaphore`).
  - Waiting with `tryWaitOnSemaphore` or interruptible `waitOnSemaphore`,
    which return `SemaphoreToken`s.
  - Releasing tokens with `releaseSemaphoreToken` or `withSemaphoreToken`.
  - Destroying semaphores with `destroyClientSemaphore` and
    `destroyServerSemaphore`.
  - Serialising names and checking versions with `semaphoreIdentifier` and
    `versionsAreCompatible`.
