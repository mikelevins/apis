# Apis: Session Handoff — Stage 6 Complete

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
transport, and now a runtime-worker for meta-protocol operations
and asynchronous request-reply correlation.

Dependencies: `bordeaux-threads`, `closer-mop`, `ironclad`,
`queues.simple-cqueue`, `usocket`. Version 0.9.6.


## Repository Structure

```
apis.asd                  — system definition
version.lisp              — "0.9.6"
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
  tcp-tests.lisp          — 12 TCP transport and listener tests
  runtime-worker-tests.lisp — runtime-worker, pending-reply, request tests
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
  handoff-stage6.md       — Stage 5 handoff
  handoff-stage7.md       — THIS FILE (Stage 6 complete)
```

Run all tests with:
```lisp
(asdf:load-system :apis/tests :force t)
(apis-tests:run-tests)
```


## What Was Done This Session

### 1. Runtime-worker class

`runtime-worker` is a subclass of `worker` that serves as the
runtime's addressable endpoint for meta-protocol operations and
request-reply correlation. It has three additional slots beyond
those inherited from `worker`:

- `owning-runtime` — a reference back to the runtime that owns it.
- `pending-replies` — a hash table mapping message-id (integer) to
  callback function, for asynchronous request-reply tracking.
- `pending-lock` — protects the pending-replies table.

The runtime-worker is a regular worker: it has a ULID, sits in the
runtime's registry, receives messages via the normal scheduling
loop, and dispatches via `handle-message`. It is not a special case
in the runtime — it is a pet worker that the runtime owns.

### 2. Runtime-worker slot on the runtime

The `runtime` class gains a `runtime-worker-instance` slot
(accessor: `runtime-worker`). This is populated automatically via
an `:after` method on `initialize-instance` for `runtime`, and
a load-time form ensures the pre-existing `*default-runtime*`
also gets one.

### 3. Pending-reply tracking

Three functions manage the callback table:

- `register-pending-reply` — maps a message-id to a callback.
- `dispatch-pending-reply` — if a cause-id matches a pending
  entry, invokes the callback with the reply message and removes
  it. Returns T if handled, NIL otherwise.
- `pending-reply-count` — returns the number of pending callbacks.

All three are thread-safe via the pending-lock.

A `receive :around` method on `runtime-worker` intercepts incoming
messages: if the message's CAUSE field matches a pending reply, the
callback is invoked instead of `handle-message`. Messages without
a matching pending reply fall through to normal dispatch.

Callbacks are one-shot: they are removed from the table after a
single invocation.

### 4. The `request` function

`request` sends a message and registers a callback for the reply:

```lisp
(request (message :to remote-runtime-worker-addr
                  :operation :version)
         (lambda (reply)
           (format t "Got version: ~A~%"
                   (getf (message-data reply) :version))))
```

If the message has nil FROM, `request` fills it with the
runtime-worker's ID so replies route back to the runtime-worker.
Returns the message ID (which will appear as CAUSE in the reply).

### 5. Meta-protocol: :version handler

The runtime-worker handles `:version` requests by sending a reply
containing the Apis version string and the runtime's local
authority. The reply's CAUSE is set to the request's message ID.

### 6. deliver-remotely fills nil FROM

`deliver-remotely` (in transport.lisp) now checks: if the
message's FROM is nil and the runtime has a runtime-worker, it
substitutes the runtime-worker's ID before enrichment. This means
messages sent from the REPL or from non-worker code get a
replyable return address automatically.

### 7. install-runtime-worker and swap-runtime-worker

`install-runtime-worker` creates a fresh runtime-worker, registers
it, and stores it in the runtime. `swap-runtime-worker` replaces
the current runtime-worker with a new one, deregistering the old
and registering the new. This enables swapping in a customized
runtime-worker with different handle-message methods during
interactive development.

### 8. Test coverage

New tests cover:
- Runtime-worker existence on the default runtime
- Runtime-worker is in the registry
- Auto-install on new runtimes
- Owning-runtime reference
- Swap replaces and re-registers correctly
- Pending-reply register and count
- Pending-reply dispatch invokes callback with data
- Dispatch returns nil for unknown cause-id
- One-shot semantics (callback invoked only once)
- receive :around routes replies to callbacks
- receive :around falls through for non-replies
- :version handler sends a reply
- request function registers callback and sends
- request fills nil FROM with runtime-worker ID
- deliver-remotely fills nil FROM from runtime-worker
- deliver-remotely preserves explicit FROM


## Key Design Decisions

All decisions from previous stages still apply, plus:

- **Pet worker, not superclass refactoring.** The runtime-worker is
  a regular worker subclass owned by the runtime, not a refactoring
  of the runtime class itself. This means the runtime-worker can be
  swapped out at any time (e.g., for one with custom handlers),
  it participates in the normal scheduling loop, and the runtime
  class remains focused on scheduling. The alternative — making
  runtime inherit from an abstract-worker class — would be a deeper
  refactoring for minimal benefit at this stage.

- **Callbacks, not promises.** The request-reply mechanism is
  callback-based: you pass a function that gets called when the
  reply arrives. This is fully asynchronous and fits the actor
  model. A promise/future convenience layer (where `request`
  returns a blockable object) can be built on top of this
  primitive later.

- **One-shot callbacks.** A pending-reply callback is invoked
  exactly once and then removed. This prevents resource leaks
  and matches the request-reply pattern (one request, one reply).
  Multi-message exchanges (dialogs) are a planned future extension.

- **CAUSE field as correlation key.** The existing `cause` field
  on messages is exactly the right mechanism for reply correlation.
  No new fields or protocols are needed. The sender records its
  message ID; the replier sets CAUSE to that ID; the sender's
  runtime-worker matches CAUSE against pending replies.

- **receive :around for reply interception.** The :around method
  on `receive` checks the CAUSE field before normal dispatch. If
  a matching pending reply exists, the callback is invoked and
  `handle-message` is never called. This is clean and efficient —
  no changes to the base `receive` method or the scheduling loop.

- **deliver-remotely fills nil FROM.** Rather than requiring every
  call site to provide a FROM address, `deliver-remotely` uses the
  runtime-worker's ID as a default. This makes REPL usage and
  non-worker code work naturally — messages always carry a
  replyable return address. Explicit FROM values are never
  overwritten.

- **Auto-install via :after method.** Every runtime gets a
  runtime-worker automatically. The `*default-runtime*` (created
  before runtime-worker.lisp loads) is handled by a load-time
  form. This ensures the runtime-worker is always available.


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
  messages share a dialog ID. A dialog log tracks the exchange for
  tracing and replay. Builds on the pending-reply mechanism.

- **Full-message encryption.** A `post-frame-transforms` slot on
  the transport class. Transforms applied after framing, reversed
  before deframing. For hiding metadata.

- **Connection management.** Reconnection on failure, connection
  pooling, keepalive for TCP transports.

- **Compression transform.** A `make-compression-transform` using
  `chipz`/`salza2`. Composed: compress → sign → encrypt.

- **Supervision and error recovery.** Worker lifecycle management,
  restart policies, error escalation.


## Documents to Read First

In priority order:
1. `src/runtime-worker.lisp` — the runtime-worker class, pending
   replies, request function, :version handler, install/swap
2. `tests/runtime-worker-tests.lisp` — all runtime-worker tests
3. `src/transport.lisp` — deliver-remotely now fills nil FROM
4. `src/runtime.lisp` — runtime-worker slot on the runtime class
5. `src/package.lisp` — current export list
