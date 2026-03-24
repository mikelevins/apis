# Apis: Session Handoff — Stages 5 & 6 Complete

## Project Location

`/Users/mikel/repos/lisp/apis`

## What Apis Is

Apis is a Common Lisp actor-style message-passing concurrency
library. Workers (actors) have ULIDs, concurrent message queues,
and behavior defined via CLOS `handle-message` methods
specialized on operation keywords. A runtime owns a fixed thread
pool and a FIFO ready-queue; scheduling threads loop pulling
ready workers and dispatching one message per turn. The design
supports distribution: addressing, serialization, a transport
layer with transform pipeline, encryption transforms, TCP
transport with cross-host delivery confirmed, and a
runtime-worker for meta-protocol operations and asynchronous
request-reply correlation.

Dependencies: `bordeaux-threads`, `closer-mop`, `ironclad`,
`queues.simple-cqueue`, `usocket`. Version 0.9.8.


## Repository Structure

```
apis.asd                  — system definition
version.lisp              — "0.9.8"
src/
  package.lisp            — package with all exports
  parameters.lisp         — global defaults
  id.lisp                 — ULID generation (monotonic single-clock)
  message.lisp            — message class and constructor
  worker.lisp             — worker class, handle-message protocol
  addressing.lisp         — URI address parsing and formatting
  runtime.lisp            — runtime, scheduling loop, send, registry,
                             transport registry, runtime-worker slot
  serialization.lisp      — payload + envelope serialization
  transport.lisp          — transport layer, transform pipeline, framing,
                             encryption and signing transforms, FROM
                             enrichment from runtime-worker
  tcp-transport.lisp      — TCP transport, connect-tcp, tcp-listener,
                             portable accept loop, robust shutdown
  runtime-worker.lisp     — runtime-worker class, pending-reply tracking,
                             request function, meta-protocol handlers
tests/
  test-framework.lisp     — minimal test framework
  tests.lisp              — core tests (IDs, messages, workers, runtime)
  serialization-tests.lisp — 25 payload round-trip and error-case tests
  addressing-tests.lisp   — 18 URI parsing/formatting tests
  envelope-tests.lisp     — 18 envelope and message serialization tests
  transport-tests.lisp    — 22 transport, transform, framing tests
  encryption-tests.lisp   — 23 encryption, signing, policy tests
  tcp-tests.lisp          — 12 TCP transport and listener tests, plus
                             cross-host test helpers with diagnostics
  runtime-worker-tests.lisp — 17 runtime-worker, pending-reply, request
                             tests
doc/
  addressing.md           — URI address scheme
  message-delivery.md     — message structure, transform pipeline
  serialization.md        — payload types, wire format, 4-stage plan
  api-version-matching.md — error reply protocol
  next-available-runtime.md — runtime architecture rationale
  initial-implementation-spec.md — spec for local-only implementation
  future-extensions.md    — serialization, distribution, observability,
                             supervision, priority
  encryption.md           — encryption transform usage, security properties
  handoff-stage7.md       — THIS FILE (Stages 5 & 6 complete)
```

All 174 tests pass on macOS and Linux. Run with:
```lisp
(asdf:load-system :apis/tests :force t)
(apis-tests:run-tests)
```

Cross-host TCP delivery confirmed between two physical machines.


## What Was Done This Session

### Stage 5: TCP Transport

**tcp-transport class.** A `transport` subclass implementing
`transport-write-bytes` / `transport-read-bytes` over TCP.
Wire protocol: 4-byte big-endian length prefix before each
framed message, so the receiver can delimit messages on the
TCP byte stream.

**connect-tcp.** Client-side constructor. Connects to host:port
and returns a ready-to-use tcp-transport.

**tcp-listener.** Accepts incoming TCP connections and delivers
received messages into a local runtime. Each accepted connection
gets a receive-loop thread. The accept loop uses
`usocket:wait-for-input` with a list argument and state-slot
checking for portable behavior across macOS and Linux.
Shutdown uses polling with fallback to `bt:destroy-thread` for
platforms where socket-close doesn't reliably interrupt blocked
I/O.

**Cross-host test helpers.** `test-tcp-connectivity` for
diagnosing connection problems (reports refused, timeout,
unreachable with actionable messages). `setup-test-receiver`,
`teardown-test-receiver`, and `send-test-message-tcp` with
full error reporting and copy-pasteable usage instructions.

**New dependency: usocket.** Portable socket library, MIT-licensed.

### Stage 6: Runtime-Worker

**runtime-worker class.** A `worker` subclass owned by a runtime.
Handles meta-protocol operations (:ping, :version) and tracks
pending request-reply callbacks via a hash table mapping
message-id to callback function.

**Pending-reply tracking.** `register-pending-reply`,
`dispatch-pending-reply`, `pending-reply-count`. Callbacks are
one-shot: invoked once, then removed. A `receive :around` method
intercepts incoming messages: if the CAUSE field matches a
pending reply, the callback fires instead of `handle-message`.

**request function.** Sends a message and registers a callback
for the reply. Fills nil FROM with the runtime-worker's ID so
replies route back correctly. Returns the message ID for
correlation.

**:version handler.** Replies with the Apis version string and
the runtime's local-authority. Reply's CAUSE is set to the
request's message ID.

**deliver-remotely fills nil FROM.** When a message has nil FROM
and the runtime has a runtime-worker, `deliver-remotely`
substitutes the runtime-worker's ID before enrichment. Messages
sent from the REPL or non-worker code get a replyable return
address automatically.

**install-runtime-worker / swap-runtime-worker.**
`install-runtime-worker` creates and registers a runtime-worker,
binding `*default-runtime*` to the target runtime during creation
so the worker's auto-registration goes to the right registry.
`swap-runtime-worker` replaces the current worker with a new one
for interactive customization.

**Auto-install via make-runtime.** `make-runtime` calls
`install-runtime-worker` (guarded by `fboundp`, since
`runtime.lisp` loads before `runtime-worker.lisp`). The
pre-existing `*default-runtime*` is patched by a load-time
form in `runtime-worker.lisp`. No `:after` method on
`initialize-instance` for `runtime` — that approach caused
cross-runtime registry pollution in tests.


## Key Design Decisions

All decisions from previous stages still apply, plus:

### TCP Transport

- **usocket, not sb-bsd-sockets.** Portable across SBCL, CCL,
  ECL, LispWorks.

- **Length-prefixed wire protocol.** TCP has no message
  boundaries; each framed message is preceded by a 4-byte
  big-endian length prefix.

- **One connection per transport.** Connection pooling deferred.

- **Per-connection threads.** Simplest concurrency model for a
  modest number of connections.

- **Portable accept loop.** `wait-for-input` with a list
  argument (not bare socket) and `usocket::state` checking.
  `:ready-only t` on server sockets is unreliable across
  platforms.

- **Polling shutdown with destroy-thread fallback.**
  `stop-tcp-listener` polls for thread exit (up to ~3 seconds),
  then destroys stubborn threads. On Linux/SBCL, closing a
  socket from another thread doesn't reliably interrupt blocked
  reads.

- **deliver-locally on receive, not send.** The receive loop
  bypasses `send`'s address resolution. Relay support would
  require routing through `send`.

### Runtime-Worker

- **Pet worker, not superclass refactoring.** The runtime-worker
  is a regular worker subclass, swappable at any time. The
  runtime class stays focused on scheduling.

- **Callbacks, not promises.** Fully asynchronous, fits the actor
  model. Promise/future is a planned convenience layer.

- **One-shot callbacks.** Prevents resource leaks. Multi-message
  dialogs are a planned future extension.

- **CAUSE field as correlation key.** No new wire-level fields
  needed. The existing cause field carries reply correlation.

- **receive :around for reply interception.** Checks CAUSE before
  normal dispatch. Clean and efficient.

- **make-runtime calls install-runtime-worker via fboundp.**
  Avoids the :after method on initialize-instance which caused
  runtime-workers to register in the wrong *default-runtime*
  during tests. The load-time form in runtime-worker.lisp
  patches the pre-existing *default-runtime*.

- **install-runtime-worker binds *default-runtime*.** Ensures
  the worker's auto-registration :after method (on the worker
  class) registers in the correct runtime.


## Linux Portability Notes

The TCP tests run slower on Linux than macOS due to the polling
approach in the accept loop and shutdown. Specifically:

- `usocket:wait-for-input` with a 0.5-second timeout adds
  wall-clock time per accept iteration.
- `stop-tcp-listener` polls at 0.1-second intervals (up to ~3
  seconds) for thread exit before falling back to
  `bt:destroy-thread`.

On macOS, socket closure more reliably interrupts blocked I/O
in other threads, so the cleanup is faster. The behavior is
correct on both platforms; only wall-clock time differs.


## Implementation Stages

```
Stage 1: Payload serialization          [COMPLETE]
    ↓
Stage 2: Envelope + addressing          [COMPLETE]
    ↓
Stage 3: Transport + transform pipeline [COMPLETE]
    ↓
Stage 4: Encryption transforms          [COMPLETE]
    ↓
Stage 5: TCP transport                  [COMPLETE]
    ↓
Stage 6: Runtime-worker                 [COMPLETE]
```


## What Could Come Next

- **Promise/future for request.** A convenience layer on top of
  the callback-based `request` that returns a blockable promise.
  Useful for REPL interaction: `(get-version remote-addr)` blocks
  and returns the version string.

- **Dialog protocol.** Multi-message exchanges where several
  messages share a dialog ID. A dialog log tracks the exchange
  for tracing and replay. Builds on the pending-reply mechanism.

- **Full-message encryption.** A `post-frame-transforms` slot on
  the transport class. Transforms applied after framing, reversed
  before deframing. For hiding metadata.

- **Connection management.** Reconnection on failure, connection
  pooling, keepalive for TCP transports.

- **Compression transform.** A `make-compression-transform` using
  `chipz`/`salza2`. Composed: compress → sign → encrypt.

- **TLS.** Wrapping TCP connections in TLS for transport-level
  encryption. Usocket supports this via `cl+ssl`.

- **Supervision and error recovery.** Worker lifecycle management,
  restart policies, error escalation.

- **Key derivation helpers.** `make-key-from-passphrase` using
  ironclad's PBKDF2.


## Documents to Read First

In priority order:
1. `src/runtime-worker.lisp` — runtime-worker class, pending
   replies, request function, :version handler, install/swap
2. `src/tcp-transport.lisp` — TCP transport, connect-tcp,
   listener, portable accept loop, robust shutdown
3. `tests/runtime-worker-tests.lisp` — all runtime-worker tests
4. `tests/tcp-tests.lisp` — TCP tests plus cross-host helpers
5. `src/transport.lisp` — deliver-remotely fills nil FROM
6. `src/runtime.lisp` — runtime-worker slot, make-runtime
   auto-install via fboundp
7. `src/package.lisp` — current export list
