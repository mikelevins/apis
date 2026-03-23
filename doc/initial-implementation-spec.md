# Apis: Initial Implementation Specification

## Scope

This document specifies the first implementation of Apis under the
next-available runtime architecture. It covers only local message
passing within a single Common Lisp process. It does not implement
serialization, distribution, encryption, or tracing.

However, every design decision is made with those future extensions
in mind. Nothing specified here will need to be torn out or
redesigned when distribution is added. The path forward is always
additive.


## Dependencies

- **bordeaux-threads** — portable threading, locks, condition
  variables.
- **queues.simple-cqueue** — concurrent queue for the runtime's
  ready-queue and per-worker message queues.

No other external libraries. In particular, the threadpool library
is no longer a dependency; the runtime replaces its role.


## Package

The system defines a single package, `apis`, which exports the
public API. Internal implementation details are not exported.

```lisp
(defpackage #:apis
  (:use #:cl)
  (:export

   ;; IDs
   #:makeid

   ;; Messages
   #:message
   #:message-id
   #:message-from
   #:message-to
   #:message-operation
   #:message-data
   #:message-timestamp
   #:message-time-to-live
   #:message-cause

   ;; Workers
   #:worker
   #:worker-id
   #:worker-description
   #:handle-message
   #:unhandled-message

   ;; Sending
   #:send

   ;; Runtime
   #:runtime
   #:make-runtime
   #:start-runtime
   #:stop-runtime
   #:runtime-running-p
   #:*default-runtime*

   ;; Dead letters
   #:*dead-letters*))
```


## Source File Organization

Files are loaded serially in this order:

| File              | Purpose                                    |
|-------------------|--------------------------------------------|
| `package.lisp`    | Package definition.                        |
| `parameters.lisp` | Global parameters and defaults.            |
| `id.lisp`         | ULID generation.                           |
| `message.lisp`    | Message class and constructor.             |
| `worker.lisp`     | Worker class and `handle-message` protocol.|
| `runtime.lisp`    | Runtime, scheduling loop, `send`, registry.|


## IDs

### `makeid` → ulid

Generate a fresh ULID. Returns a 128-bit integer composed of 48
bits of millisecond timestamp and 80 bits of randomness.

For the initial implementation, a simplified ULID generator is
acceptable: shift the millisecond timestamp left by 80 bits and OR
in a random 80-bit value. This produces values with the correct
structure, sort order, and collision resistance.

A `format-id` function converts a ULID integer to its 26-character
Crockford base32 string representation. A `parse-id` function
reverses the conversion. These are needed for printing and for
future serialization, but are not part of the critical path for
local message delivery.


## Messages

### Class: `message`

Slots:

| Slot             | Reader                 | Type               | Default               |
|------------------|------------------------|--------------------|-----------------------|
| `id`             | `message-id`           | integer (ULID)     | `(makeid)`            |
| `from`           | `message-from`         | ULID or nil        | `nil`                 |
| `to`             | `message-to`           | ULID               | required              |
| `operation`      | `message-operation`    | keyword            | required              |
| `data`           | `message-data`         | plist or nil       | `nil`                 |
| `timestamp`      | `message-timestamp`    | integer            | `(get-universal-time)`|
| `time-to-live`   | `message-time-to-live` | integer (seconds)  | `*default-ttl*`       |
| `cause`          | `message-cause`        | ULID or nil        | `nil`                 |

The `from` and `to` fields hold worker IDs (ULIDs), not worker
object references. This is a deliberate departure from the current
implementation, where `to` holds the worker object itself. Using
IDs makes messages self-contained values that do not depend on live
object references, which is essential for future serialization and
distribution.

### Constructor: `message`

```lisp
(message :to target-id
         :operation :some-op
         &key from data cause)
```

Returns a new `message` instance. The `:to` and `:operation`
arguments are required. All other fields have sensible defaults.

### Print representation

Messages print readably with their ID (in hex or base32),
operation, and from/to addresses. The exact format is not
specified; clarity in REPL output is the goal.


## Workers

### Class: `worker`

Slots:

| Slot             | Accessor               | Type                   | Default                |
|------------------|------------------------|------------------------|------------------------|
| `id`             | `worker-id`            | integer (ULID)         | `(makeid)`             |
| `description`    | `worker-description`   | string or nil          | `nil`                  |
| `message-queue`  | —  (internal)          | concurrent queue       | fresh `simple-cqueue`  |
| `state`          | —  (internal)          | keyword                | `:idle`                |

The `message-queue` and `state` slots are internal to the
implementation and not exported. User code never inspects or
modifies them.

The `state` slot holds one of three keywords: `:idle`, `:ready`,
or `:running`. Transitions are as specified in
`next-available-runtime.md`. Only the runtime modifies this slot.

### Worker registration

When a worker is created, it is automatically registered in the
default runtime's registry (a hash table mapping ULID → worker).
This happens in an `:after` method on `initialize-instance` or in
a custom constructor function — the mechanism is not specified, but
the invariant is: **every worker is registered in exactly one
runtime's registry from the moment of creation.**

A future extension may allow specifying which runtime to register
in. For now, all workers register in `*default-runtime*`.

### `handle-message` (generic function)

```lisp
(defgeneric handle-message (worker message operation data))
```

Dispatched by the runtime when a worker receives a turn. The
`operation` argument is the message's operation keyword; `data` is
the message's data plist. This enables EQL specialization on
operation:

```lisp
(defmethod handle-message ((w my-worker) msg (op (eql :greet)) data)
  (format t "Hello from ~A~%" (getf data :name)))
```

**Default method.** The default method signals an
`unhandled-message` condition of type `error`.

**Built-in :ping handler.** A method specialized on `(eql :ping)`
is provided on the base `worker` class. It prints a brief
acknowledgment. This serves as a smoke test and a minimal example.

### `unhandled-message` (condition)

A condition of type `error`, carrying the unhandled message. Signaled
by the default `handle-message` method. The runtime's scheduling
loop catches this (see below) so that an unhandled message does not
crash a runtime thread.


## Runtime

### Class: `runtime`

Slots:

| Slot             | Accessor               | Type                     | Default            |
|------------------|------------------------|--------------------------|--------------------|
| `threads`        | —  (internal)          | list of threads          | `nil`              |
| `thread-count`   | `runtime-thread-count` | positive integer         | `4`                |
| `ready-queue`    | —  (internal)          | concurrent queue         | fresh queue         |
| `ready-lock`     | —  (internal)          | lock                     | fresh lock          |
| `ready-condvar`  | —  (internal)          | condition variable       | fresh condvar       |
| `registry`       | —  (internal)          | hash table (ULID → worker)| fresh table       |
| `registry-lock`  | —  (internal)          | lock                     | fresh lock          |
| `running-p`      | `runtime-running-p`    | boolean                  | `nil`              |
| `dead-letters`   | —  (internal)          | list or vector           | empty              |

Most slots are internal. The user-facing surface is:

- `make-runtime &key thread-count` — create a runtime.
- `start-runtime runtime` — spawn the scheduling threads.
- `stop-runtime runtime` — shut down gracefully.
- `runtime-running-p runtime` — predicate.

### The default runtime: `*default-runtime*`

A global variable holding a runtime instance. Initialized and
started automatically when the system is loaded. Its thread count
defaults to 4 but can be configured before loading by binding
`*default-runtime-thread-count*` (or similar parameter).

The existence of the default runtime means that simple programs
never mention runtimes at all. Create workers, define handlers,
send messages.

### The scheduling loop

Each runtime thread executes the same function:

```
loop:
  1. Acquire ready-lock.
  2. While ready-queue is empty and running-p is true:
       wait on ready-condvar (releasing ready-lock).
  3. If running-p is false: release lock and exit the loop.
  4. Dequeue one worker from ready-queue.
  5. Set worker state to :running.
  6. Release ready-lock.
  7. Pop one message from the worker's message-queue.
  8. Call (receive worker message), which dispatches to
     handle-message, inside a handler-case that catches errors.
  9. Acquire ready-lock.
  10. If the worker's message-queue is non-empty:
        set state to :ready, push worker onto ready-queue.
      Else:
        set state to :idle.
  11. Release ready-lock.
  12. Go to 1.
```

Error handling in step 8: if `handle-message` signals an error,
the scheduling loop catches it, issues a warning (via `warn`), and
continues. The worker is not removed from the system. The failed
message is logged or filed as a dead letter at the implementor's
discretion. The scheduling thread must never die due to a handler
error.

### Thread safety

The ready-queue and worker state transitions are protected by
`ready-lock`. This is the single synchronization point in the
system. Per-worker message queues are concurrent queues
(`simple-cqueue`) and are safe for concurrent push (from any
thread calling `send`) and pop (from the runtime thread that has
claimed the worker).

The registry is protected by `registry-lock`, which is acquired
only during worker creation and ID lookup — both infrequent
relative to message flow.

No per-worker locks or semaphores exist. This is a deliberate
simplification over the current design.

### `start-runtime` / `stop-runtime`

`start-runtime` spawns `thread-count` threads, each running the
scheduling loop. Sets `running-p` to true.

`stop-runtime` sets `running-p` to false, broadcasts on
`ready-condvar` to wake all waiting threads, then joins all
threads. After `stop-runtime` returns, no scheduling threads are
running. Messages sent after shutdown are filed as dead letters.

### Shutdown and worker cleanup

`stop-runtime` does not destroy workers or drain their queues.
Workers continue to exist with whatever messages remain queued. A
subsequent `start-runtime` (or a new runtime) could resume
processing them. This preserves the principle that workers are
values with identity, not threads with lifecycles.


## `send`

### Signature

```lisp
(defgeneric send (message))
```

Or, if more convenient for the implementation:

```lisp
(defun send (message &optional (runtime *default-runtime*))
  ...)
```

### Behavior

1. Extract the `to` field (a ULID) from the message.
2. Look up the ULID in the runtime's registry.
3. If not found: file the message as a dead letter and return.
4. If found: push the message onto the worker's message-queue.
5. Acquire ready-lock.
6. If the worker's state is `:idle`:
     set state to `:ready`, push onto ready-queue,
     signal ready-condvar.
7. Release ready-lock.

`send` is safe to call from any thread, including from within a
`handle-message` method (which is the normal way actors send
messages to each other).


## Dead Letters

Messages that cannot be delivered — because the target ULID is not
in the registry — are collected in the runtime's dead letter store.
Each dead letter records the original message and a reason string.

The dead letter store is accessible for inspection (e.g., from the
REPL) but its exact interface is not specified here. A simple
adjustable vector is sufficient for the initial implementation.


## Parameters

| Parameter                          | Default | Purpose                              |
|------------------------------------|---------|--------------------------------------|
| `*default-ttl*`                    | 600     | Default message time-to-live.        |
| `*default-runtime-thread-count*`   | 4       | Thread count for the default runtime.|
| `*default-runtime*`                | —       | The global runtime instance.         |


## ASDF System Definition

```lisp
(asdf:defsystem #:apis
  :serial t
  :description "Apis: actors passing messages"
  :author "mikel evins <mevins@me.com>"
  :license "Apache 2.0"
  :version (:read-file-form "version.lisp")
  :depends-on (:bordeaux-threads
               :queues.simple-cqueue)
  :components ((:module "src"
                :serial t
                :components ((:file "package")
                             (:file "parameters")
                             (:file "id")
                             (:file "message")
                             (:file "worker")
                             (:file "runtime")))))
```

Note: `:threadpool` is removed from `:depends-on`.


## What Is Deliberately Deferred

The following are specified in the design documents but not
implemented in this initial version:

- **Serialization.** Messages are Lisp objects passed by reference
  on a local queue. The message structure (ID-based addressing,
  permitted payload types) is designed for serializability, but no
  serializer is implemented yet.
- **Payload validation.** The permitted type set is defined but not
  enforced at runtime. Validation will be added with the
  serializer.
- **URI addresses.** The `from` and `to` fields hold bare ULIDs.
  URI parsing and the authority/path structure are deferred to the
  distribution implementation.
- **Transform pipeline.** No serialization means no transforms.
  The clean separation between local delivery (push object onto
  queue) and distributed delivery (serialize, transform, transport)
  means the pipeline can be inserted later without modifying local
  delivery.
- **Tracing and observability.** The scheduling loop is the natural
  instrumentation point. Hooks can be added later without
  structural changes.
- **Supervision.** Error handling in the scheduling loop is minimal
  (catch and warn). A supervision policy can replace this later.
- **`service-worker`.** The current codebase has a `service-worker`
  subclass that runs a background function alongside message
  handling. This can be reintroduced as a worker subclass whose
  background function is submitted to the runtime, but it is not
  part of the initial implementation.


## Acceptance Criteria

The implementation is complete when:

1. A runtime can be created and started with a configurable number
   of threads.
2. Workers can be created and are automatically registered.
3. Messages can be constructed with `message` and sent with `send`.
4. The runtime dispatches messages to workers fairly (FIFO
   ready-queue, one message per turn).
5. `handle-message` methods specialized on worker subclass and
   operation keyword are invoked correctly.
6. The actor serialization invariant holds: at most one message is
   processed per worker at any time, even under concurrent load.
7. Unhandled messages (no matching `handle-message` method) signal
   a condition that is caught by the runtime without crashing the
   scheduling thread.
8. Messages to unknown worker IDs are filed as dead letters.
9. `stop-runtime` shuts down cleanly: all scheduling threads exit,
   no orphaned threads remain.
10. The default runtime starts automatically on system load, so
    that a minimal program requires only worker creation, handler
    definition, and `send`.

### Smoke test

The following session at the REPL should work:

```lisp
;; Load the system
(asdf:load-system :apis)

;; Define a worker subclass
(defclass greeter (apis:worker) ())

;; Define a handler
(defmethod apis:handle-message ((w greeter) msg (op (eql :greet)) data)
  (format t "~&Hello, ~A!~%" (getf data :name)))

;; Create workers
(defvar *g1* (make-instance 'greeter))
(defvar *g2* (make-instance 'greeter))

;; Send messages
(apis:send (apis:message :to (apis:worker-id *g1*)
                         :operation :greet
                         :data '(:name "Alice")))

(apis:send (apis:message :to (apis:worker-id *g2*)
                         :operation :greet
                         :data '(:name "Bob")))

;; Output (possibly interleaved):
;; Hello, Alice!
;; Hello, Bob!

;; Ping (built-in handler)
(apis:send (apis:message :to (apis:worker-id *g1*)
                         :operation :ping))

;; Shutdown
(apis:stop-runtime apis:*default-runtime*)
```
