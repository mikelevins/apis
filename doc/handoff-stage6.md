# Apis: Session Handoff — Stage 5 Complete

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
layer with transform pipeline, encryption transforms, and now
TCP transport with a listener for receiving messages over the
network.

Dependencies: `bordeaux-threads`, `closer-mop`, `ironclad`,
`queues.simple-cqueue`, `usocket`. Version 0.9.4.


## Repository Structure

```
apis.asd                  — system definition
version.lisp              — "0.9.4"
src/
  package.lisp            — package with all exports
  parameters.lisp         — global defaults
  id.lisp                 — ULID generation (monotonic single-clock)
  message.lisp            — message class and constructor
  worker.lisp             — worker class, handle-message protocol
  addressing.lisp         — URI address parsing and formatting
  runtime.lisp            — runtime, scheduling loop, send, registry,
                             transport registry
  serialization.lisp      — payload + envelope serialization
  transport.lisp          — transport layer, transform pipeline, framing,
                             encryption and signing transforms
  tcp-transport.lisp      — TCP transport, connect-tcp, tcp-listener
tests/
  test-framework.lisp     — minimal test framework (define-test, check,
                             check-equal, check-condition)
  tests.lisp              — core tests (IDs, messages, workers, runtime,
                             send, smoke test)
  serialization-tests.lisp — 25 payload round-trip and error-case tests
  addressing-tests.lisp   — 18 URI parsing/formatting tests
  envelope-tests.lisp     — 18 envelope and message serialization tests
  transport-tests.lisp    — 22 transport, transform, framing tests
  encryption-tests.lisp   — 23 encryption, signing, policy tests
  tcp-tests.lisp          — TCP transport and listener tests, plus
                             cross-host test helpers
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
  handoff-stage2.md       — Stage 1 handoff
  handoff-stage3.md       — Stage 2 handoff
  handoff-stage4.md       — Stage 3 handoff
  handoff-stage5.md       — Stage 4 handoff
  handoff-stage6.md       — THIS FILE (Stage 5 complete)
```

Run all tests with:
```lisp
(asdf:load-system :apis/tests :force t)
(apis-tests:run-tests)
```


## What Was Done This Session

### 1. TCP transport class

`tcp-transport` is a subclass of `transport` that implements
`transport-write-bytes` and `transport-read-bytes` over a TCP
socket. The wire protocol prepends a 4-byte big-endian length
prefix to each framed message, so the receiver can delimit
messages on the TCP byte stream.

The framed message (produced by `frame-message`) is treated as
opaque bytes by the TCP layer. The length prefix is a second
layer of framing specific to the stream protocol — the internal
envelope/payload framing is unchanged.

`transport-close` closes the socket via `usocket:socket-close`,
with error suppression for idempotent close.

### 2. `connect-tcp`

Creates a `tcp-transport` by connecting to a remote host:port.
Accepts `:transforms` and `:local-authority` keyword arguments,
passed through to the transport. Returns the connected transport
ready for `transport-send`.

```lisp
(defvar *tr* (connect-tcp "remote-host" 9100
               :transforms (list sign-xf encrypt-xf)
               :local-authority "myhost:9100"))
```

### 3. TCP listener

`tcp-listener` is a class that accepts incoming TCP connections
and delivers received messages into a local runtime. Each
accepted connection gets its own receive-loop thread.

Lifecycle:
- `start-tcp-listener` binds the server socket and spawns an
  accept loop thread.
- The accept loop uses `usocket:wait-for-input` with a 1-second
  timeout for clean shutdown detection.
- Each accepted connection is wrapped in a `tcp-transport` and
  gets a receive-loop thread.
- `stop-tcp-listener` sets running-p to nil, closes the listen
  socket (unblocks accept), closes all accepted connections
  (unblocks receive loops), and joins all threads.

The receive loop calls `transport-receive` (which handles the
full reverse pipeline: read → deframe → reverse-transform →
deserialize) and then `deliver-locally` with the worker-id
extracted from the TO address.

### 4. TCP wire protocol

```
[4 bytes: frame-length N, big-endian uint32]
[N bytes: framed message]
```

This is layered on top of the existing framing protocol. Three
helper functions handle the wire format:
- `write-uint32-to-stream` — writes 4 bytes big-endian
- `read-uint32-from-stream` — reads 4 bytes, signals END-OF-FILE
  on premature close
- `read-exact-octets` — reads exactly N bytes with a loop to
  handle partial reads, signals TRANSPORT-ERROR on premature close

### 5. New dependency: usocket

Added to `apis.asd` `:depends-on`. Usocket is the standard
portable Common Lisp socket library, MIT-licensed. Used for:
`socket-connect`, `socket-listen`, `socket-accept`, `socket-close`,
`socket-stream`, `wait-for-input`.

### 6. Cross-host test helpers

Three convenience functions in the test file for manual testing
between two machines:

- `setup-test-receiver` — creates a runtime, test worker, and
  listener. Prints the worker ULID to stdout.
- `teardown-test-receiver` — stops listener and runtime.
- `send-test-message-tcp` — connects, sends one message, closes.

Usage:
```lisp
;; Machine A (receiver):
(asdf:load-system :apis/tests :force t)
(apis-tests:setup-test-receiver :port 9100)
;; prints: Worker ULID: 01ARZ3NDEKTSV4RRFFQ69G5FAV

;; Machine B (sender):
(asdf:load-system :apis/tests :force t)
(apis-tests:send-test-message-tcp
  "machine-a.local" 9100
  "01ARZ3NDEKTSV4RRFFQ69G5FAV"
  '(:greeting "hello from B"))

;; Machine A (verify):
(queues:qpop (apis-tests::tcp-test-received
               apis-tests:*cross-host-worker*))
```

### 7. Test coverage

New tests cover:
- Raw octet round-trip over TCP
- Full pipeline (serialize → transform → frame → TCP → deframe
  → reverse → deserialize) without listener
- Pipeline with sign-then-encrypt over TCP
- Multiple messages on a single TCP connection
- `connect-tcp` convenience function
- Listener end-to-end delivery (message arrives at worker)
- Listener with all message fields preserved (strings, integers,
  floats, FROM enrichment)
- Listener with encryption transforms
- Listener with per-operation transform policy
- Listener multiple messages delivery
- Listener clean start/stop lifecycle
- Transport double-close safety


## Key Design Decisions

All decisions from previous stages still apply, plus:

- **`usocket`, not raw sb-bsd-sockets.** Usocket is the portable
  abstraction. It works across SBCL, CCL, ECL, LispWorks, and
  others. Using implementation-specific socket APIs would limit
  portability for no benefit.

- **Length-prefixed wire protocol.** TCP is a byte stream with no
  message boundaries. Each framed message is preceded by a 4-byte
  big-endian length prefix so the receiver knows exactly how many
  bytes to read. This is a second layer of framing (the first
  being the internal envelope/payload framing), but TCP requires
  it because the stream has no concept of message boundaries.

- **One connection per transport.** A `tcp-transport` wraps exactly
  one socket. Connection pooling and multiplexing are deferred.
  For the current use case (point-to-point transport between two
  runtimes), one connection is sufficient. The transport registry
  already maps authority strings to transports, so multiple
  connections to different hosts are naturally supported.

- **Listener spawns per-connection threads.** Each accepted
  connection gets its own receive-loop thread. This is the
  simplest concurrency model and is appropriate for a modest
  number of connections. An event-driven model (select/epoll)
  would scale better but adds significant complexity.

- **`wait-for-input` with timeout for the accept loop.** Rather
  than blocking indefinitely on `socket-accept`, the accept loop
  uses `usocket:wait-for-input` with a 1-second timeout. This
  lets the loop check `running-p` periodically for clean shutdown
  without needing to forcibly close the listen socket (though we
  do close it anyway as belt-and-suspenders).

- **`deliver-locally` on receive, not `send`.** The receive loop
  extracts the worker-id from the TO address and calls
  `deliver-locally` directly, bypassing `send`'s address
  resolution. This avoids the receive loop trying to look up a
  transport for the TO address's authority (which would be the
  local host). Relay support (forwarding to yet another host)
  would require routing through `send`, but that is a future
  extension.

- **Transforms on the listener, not negotiated.** The listener's
  transforms are set at creation time and apply to all accepted
  connections. There is no key negotiation protocol. Both sides
  must be configured with matching transforms and keys out of
  band. This matches the existing pre-shared-key model from
  Stage 4.

- **read-exact-octets loops on partial reads.** TCP can deliver
  fewer bytes than requested in a single read. The helper
  function loops until all requested bytes are received. This is
  essential for correctness — without it, large messages would
  be silently truncated.

- **Graceful shutdown on EOF.** When the peer closes the
  connection, `read-byte` signals `end-of-file`. The receive
  loop catches this and cleanly closes its side of the
  connection. No error is logged for a clean disconnect.


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
```


## What Could Come Next

- **Full-message encryption.** A `post-frame-transforms` slot on
  the transport class. Transforms applied after framing, reversed
  before deframing. For hiding metadata (who is communicating,
  not just what).

- **Key derivation helpers.** `make-key-from-passphrase` using
  ironclad's PBKDF2. Convenience for users who don't want to
  manage raw octet keys.

- **Compression transform.** A `make-compression-transform` using
  `chipz`/`salza2`. Composed with encryption: compress → sign →
  encrypt.

- **Connection management.** Reconnection on failure, connection
  pooling, keepalive. The current model is one-shot: if a
  connection drops, the transport is dead. Automatic reconnection
  would make the system more resilient.

- **TLS.** Wrapping TCP connections in TLS for transport-level
  encryption (complementing the application-level encryption
  transforms). Usocket supports this via `cl+ssl`.

- **Supervision and error recovery.** Worker lifecycle management,
  restart policies, error escalation.


## Documents to Read First

In priority order:
1. `src/tcp-transport.lisp` — the TCP transport, connect-tcp,
   listener, accept loop, receive loop
2. `tests/tcp-tests.lisp` — all TCP tests plus cross-host helpers
3. `doc/encryption.md` — transform usage (unchanged, still applies)
4. `doc/message-delivery.md` — pipeline spec (unchanged, still applies)
5. `src/transport.lisp` — base transport class, pipeline methods
6. `src/package.lisp` — current export list
