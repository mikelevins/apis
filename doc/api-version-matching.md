# Apis: API Version Matching

## Overview

When two Apis runtimes communicate across process or network
boundaries, their interfaces may not agree. A message may name an
operation the receiver doesn't handle, carry a serializable type
the receiver doesn't have, or use a payload shape the handler
doesn't expect. This document specifies how mismatches are
detected, reported to both sides, and — in interactive development
— resolved without restarting either process.

The design rests on three mechanisms:

- **Error reply protocol.** A small set of reserved operations that
  the runtime uses to report problems back to the sender.

- **Runtime error policy.** A policy object that controls whether
  errors propagate to the debugger (for interactive development) or
  are caught and logged (for deployed services).

- **Capability advertisement.** An optional protocol for runtimes
  to describe their interfaces to each other at connection time.


## Error Sites

There are three distinct points in the receive path where a
mismatch can manifest:

### 1. Deserialization errors

The incoming message cannot be reconstructed into Lisp objects.
Causes include:

- A serialized `:obj` tag names a class that doesn't exist in the
  receiving process.
- A serialized symbol references a package that doesn't exist.
- The wire format is malformed or uses an unrecognized tag.

The raw message bytes are available; the Lisp-level message is not.
This is always non-fatal to the runtime — the message is bad, not
the system.

### 2. Dispatch errors

The message deserializes successfully but cannot be delivered.
Causes include:

- The target worker ID is not in the local registry (already
  handled by the dead-letter mechanism).
- The worker exists but has no `handle-message` method for the
  given operation (the existing `unhandled-message` condition).

### 3. Handler errors

The message is delivered and the handler runs but signals an error.
This is application code; severity depends entirely on what the
application is doing. The current scheduling loop already catches
these errors via `handler-case` and issues a `warn`.


## Error Reply Protocol

When the receiver detects a mismatch, it should inform the sender
so that the sender can surface the problem through normal condition
handling. This is accomplished with reserved system operations
distinguished by an `apis/` prefix in the operation keyword.

### `:apis/error`

Sent by the receiver back to the sender when a message cannot be
processed. Fields:

| Data key        | Type    | Purpose                                |
|-----------------|---------|----------------------------------------|
| `:reason`       | keyword | Category of failure (see below).       |
| `:description`  | string  | Human-readable explanation.            |
| `:original-op`  | keyword | The operation of the failed message.   |
| `:detail`       | plist   | Additional diagnostic data (optional). |

The message's `cause` field references the original message's ID,
so the sender can correlate the error with the send that caused it.

**Dependency on the `from` field.** The error reply is sent to the
address in the original message's `from` field. In the current
local-only implementation, `from` is commonly `nil` — the smoke
test and most local usage omit it, because local workers don't
need a return address to function. When the transport layer is
implemented, it should **automatically populate `from`** with the
sender's full address (including authority component) on all
outbound messages. This ensures that error replies always have
somewhere to go without requiring application code to set `from`
explicitly. If `from` is nil on a message that triggers an error,
the receiver files the dead letter but cannot send a reply; the
sender learns nothing. This is acceptable for local sends (where
the developer sees the warning directly) but unacceptable for
distributed sends, hence the transport-layer auto-population
requirement. This is a transport concern, not a serialization
concern, but it is noted here because the error reply protocol
depends on it.

#### Reason keywords

| Reason                    | Meaning                                    |
|---------------------------|--------------------------------------------|
| `:unknown-type`           | Deserialization found an unknown class.     |
| `:unknown-package`        | Deserialization found an unknown package.   |
| `:malformed-message`      | Wire format could not be parsed.            |
| `:unknown-worker`         | Target worker ID not found.                 |
| `:unhandled-operation`    | Worker has no handler for the operation.    |
| `:handler-error`          | Handler signaled an error.                  |

### `:apis/ping`

The existing `:ping` operation, promoted to a system-level
operation. Used for liveness checking between runtimes. A runtime
that receives `:apis/ping` replies with `:apis/pong`.

### `:apis/describe`

A runtime responds to this operation with a description of its
capabilities:

| Data key         | Type    | Purpose                                |
|------------------|---------|----------------------------------------|
| `:apis-version`  | string  | The Apis library version.              |
| `:workers`       | list    | List of plists, each describing a      |
|                  |         | worker class and its handled operations.|

The response operation is `:apis/description`.

This is optional — runtimes do not need to exchange capability
descriptions before communicating. The error reply mechanism is
the safety net that always works. Capability advertisement is a
convenience for catching mismatches early, for tooling, and for
building dashboards of distributed system topology.

### Namespace convention

All system operations use the `apis/` prefix. Application code
should not define operations with this prefix. This is a naming
convention, not an enforcement mechanism — the runtime does not
reject application messages with `apis/`-prefixed operations, but
they may collide with future system operations.


## Runtime Error Policy

### The problem

The error sites described above signal conditions. In interactive
development, these conditions should propagate to the debugger
with restarts available, so the developer can inspect the problem,
fix it (define a missing class, add a handler), and continue
without restarting. In a deployed service, these conditions must be
caught and logged so the runtime continues operating.

This is the same problem Hunchentoot solves with its
`*catch-errors-p*` variable, which controls whether HTTP handler
errors drop into the debugger or are suppressed so the server
keeps serving. Apis generalizes this to a policy object because
there are multiple error sites that may warrant different
treatment.

### Policy protocol

```lisp
(defgeneric handle-runtime-error (policy error context message)
  (:documentation "Called when the runtime encounters an error during
distributed message processing.

POLICY is the active runtime error policy.
ERROR is the condition that was signaled.
CONTEXT is a keyword indicating where the error occurred:
  :deserialization, :dispatch, or :handler.
MESSAGE is the message being processed, or NIL if the message
  could not be reconstructed (deserialization failure).

The method should handle the error and return normally to continue
operation. If it declines to handle the error — by calling
CALL-NEXT-METHOD with no applicable next method, or by not
returning — the condition propagates normally (to the debugger,
or to the scheduling loop's existing handler-case)."))
```

### Built-in policies

**`development-policy`** — the default method does not handle the
error, allowing the condition to propagate to the debugger. All
restarts established by the transport and dispatch layers are
available. The developer can:

- Inspect the failed message and the condition.
- Define a missing class or handler at the REPL.
- Invoke a `retry` restart to reprocess the message.
- Invoke a `skip` restart to discard the message and continue.

**`service-policy`** — catches the error, logs it (via a
configurable logging function), files the original message as a
dead letter with the error context, sends an `:apis/error` reply
to the sender if a `from` address is available, and returns
normally. The transport/dispatch thread continues processing the
next message.

### Thread-blocking under development policy

When `development-policy` is active and an error propagates to the
debugger, the runtime thread that encountered the error is blocked
until the developer invokes a restart or aborts. This is the
intended behavior — the developer needs a live stack frame with
the condition and all restarts available in order to diagnose and
fix the problem interactively. However, it means that one of the
runtime's scheduling threads (or transport receive threads) is
occupied for the duration of the debugging session.

This is the same tradeoff Hunchentoot makes: in development mode,
a handler error blocks the thread that was serving the request,
and the developer must act to unblock it. In Apis, the remaining
runtime threads continue processing other workers' messages, so
the system does not halt entirely — but sustained capacity is
reduced by one thread. If multiple errors propagate
simultaneously, multiple threads block, and throughput degrades
further.

The implication is that `development-policy` is appropriate when a
developer is present at the REPL and can respond to debugger
prompts. For unattended operation — CI test runs, staging
environments, production — `service-policy` (or a custom policy)
should be used. The policy variable makes this an explicit,
auditable configuration choice rather than an implicit behavior
that might surprise the user.

### Policy scope: per-runtime, not global

The error policy is stored per-runtime rather than in a single
global variable. A process may host multiple runtimes — for
example, a production runtime handling live traffic alongside a
development runtime used for interactive experimentation or
debugging. Per-runtime policy allows the production runtime to use
`service-policy` (errors are caught, logged, and replied to) while
the development runtime uses `development-policy` (errors
propagate to the debugger). A global policy would force a single
error-handling strategy on all runtimes in the process, which is
unnecessarily restrictive.

The runtime class gains an `error-policy` slot:

```lisp
(defclass runtime ()
  (...
   (error-policy :accessor runtime-error-policy
                 :initform (make-instance 'development-policy)
                 :initarg :error-policy))
  ...)
```

The scheduling loop and transport receive path consult
`(runtime-error-policy runtime)` rather than a global variable.
For convenience, a global default can still be provided via a
parameter like `*default-runtime-error-policy*`, used as the
initform, so that a program-wide policy can be set before any
runtimes are created:

```lisp
(defparameter *default-runtime-error-policy*
  (make-instance 'development-policy))
```

### Interaction with the condition system

The policy is invoked inside `handler-bind`, not `handler-case`.
The condition remains active on the stack when the policy method
runs, so restarts established by the caller are available. This
means:

- A `development-policy` simply declines, and the debugger
  presents the restarts.
- A `service-policy` can invoke `skip` or `continue` restarts
  programmatically.
- A custom policy can make sophisticated decisions: retry
  deserialization after loading a class definition from a known
  location, escalate certain error categories while suppressing
  others, notify an external monitoring system, etc.

### Established restarts

The transport receive path and dispatch path establish the
following restarts at each error site:

| Restart   | Available at             | Effect                          |
|-----------|--------------------------|---------------------------------|
| `retry`   | All error sites          | Reprocess the message from the  |
|           |                          | current stage.                  |
| `skip`    | All error sites          | Discard the message and         |
|           |                          | continue processing.            |
| `use-value`| Deserialization errors  | Substitute a replacement Lisp   |
|           |                          | value for the one that failed   |
|           |                          | to deserialize.                 |

### Custom policies

Users define custom policies by subclassing `development-policy`
or `service-policy` and specializing `handle-runtime-error`. For
example, a policy that escalates handler errors but suppresses
deserialization errors:

```lisp
(defclass selective-policy (service-policy) ())

(defmethod handle-runtime-error ((policy selective-policy)
                                 error
                                 (context (eql :handler))
                                 message)
  ;; Let handler errors propagate to the debugger
  ;; by declining to handle them.
  nil)

;; Deserialization and dispatch errors inherit the
;; service-policy behavior: log, dead-letter, continue.
```

### Relationship to the existing scheduling loop

The current scheduling loop already catches handler errors with
`handler-case` and issues `warn`. This is effectively a hardcoded
service policy for handler errors in the local case. When the
policy mechanism is implemented, the scheduling loop's error
handling should be unified with it: consult the runtime's
`error-policy` rather than unconditionally catching and warning.
This preserves current behavior (the default `development-policy`
lets errors propagate, and the scheduling loop's `handler-case`
catches them as it does today) while enabling the developer to
override the behavior when a transport is active.


## Sender-Side Error Handling

When a sender receives an `:apis/error` reply, the runtime should
surface it through the condition system so that application code
can respond.

### `remote-message-error` condition

A condition of type `warning` (or optionally `error`, controlled
by policy) that carries the diagnostic data from the error reply:

| Accessor            | Value                                       |
|---------------------|---------------------------------------------|
| `remote-error-reason`| The `:reason` keyword from the reply.      |
| `remote-error-description`| The human-readable description.       |
| `remote-error-original-operation`| The operation that failed.     |
| `remote-error-cause`| The ULID of the original message.           |
| `remote-error-detail`| Additional diagnostic plist.               |

By default, `remote-message-error` is a `warning`, so it is
reported but does not interrupt execution. The sender's error
policy can promote it to an error (entering the debugger) when
running interactively.

### Restarts on the sender side

| Restart    | Effect                                          |
|------------|-------------------------------------------------|
| `resend`   | Re-send the original message (after the         |
|            | developer has modified the data or the remote    |
|            | side has updated its definitions).               |
| `continue` | Ignore the error and continue.                  |


## Interactive Development Workflow

The following scenario illustrates how the mechanisms work together
during development:

1. Developer A is working on process A. Developer B is working on
   process B. Both processes are running with their runtimes'
   error policies set to `development-policy`.

2. A defines a new serializable class `order` and sends a message
   to B containing an `order` instance.

3. B's transport layer attempts to deserialize the message. The
   deserializer encounters `(:obj "MY-APP" "ORDER" ...)` but
   process B does not have the `order` class. It signals a
   condition.

4. Because B's runtime is using `development-policy`, the
   condition propagates. B's developer lands in the debugger and
   sees: `Class MY-APP:ORDER not found during deserialization.`
   Available restarts: `retry`, `skip`.

5. B's developer evaluates the `order` class definition at the
   REPL (perhaps loading it from a shared source file), then
   invokes `retry`.

6. The message deserializes successfully. B's worker receives it,
   but has no handler for the `:place-order` operation. The
   `unhandled-message` condition is signaled.

7. B's developer sees the condition, defines the handler at the
   REPL, invokes `retry`. The message is processed.

8. No `:apis/error` is ever sent back to A, because every error
   was resolved interactively on B's side. The transport layer
   would have auto-populated A's address in the `from` field, so
   an error reply was possible — but it was never needed.

In a deployed service, the same sequence would instead: catch each
error, log it, file the message as a dead letter, and send
`:apis/error` back to A — all without interrupting B's operation.


## Design Notes

**Why a policy object rather than a flag?** A boolean
`*catch-errors-p*` would suffice for the two common cases
(development and production). But a policy object as a CLOS
generic function dispatch target allows users to write policies
that make context-dependent decisions — suppressing some error
categories while escalating others, implementing retry logic,
integrating with external monitoring. The two built-in policies
cover the common cases; the extension mechanism covers everything
else.

**Why reserved operations rather than a separate channel?** The
error replies travel as ordinary Apis messages using the same
transport as application messages. This means no additional
infrastructure — no side channel, no out-of-band protocol. The
`:apis/` prefix is a namespace convention that keeps system
messages distinguishable without requiring the runtime to treat
them differently at the transport level. A worker can even define
a handler for `:apis/error` to implement custom error-handling
logic at the application level.

**Why `handler-bind` rather than `handler-case`?** `handler-bind`
preserves the dynamic environment at the point of the error,
including all restarts. `handler-case` unwinds the stack before
the handler runs, destroying the restarts. Since the whole point
of the development policy is to let the developer invoke restarts,
`handler-bind` is essential. The service policy can still invoke
restarts programmatically (e.g., calling `skip`) because they
remain available.

**Why is capability advertisement optional?** Because the error
protocol is sufficient on its own. Two runtimes can communicate
without ever exchanging capability descriptions; mismatches are
detected and reported as they occur. Capability advertisement is
useful for tooling (dashboards, topology maps), for pre-flight
checks in deployment pipelines, and for early warnings during
development. But it is not a prerequisite for communication, and
making it mandatory would add complexity and a synchronization
step to connection establishment.


## Impact on the Core

**Scheduling loop.** The existing `handler-case` around `receive`
in the scheduling loop should be refactored to use `handler-bind`
with restarts, consulting the runtime's error policy. This is a
small change that preserves current default behavior while enabling
policy-driven error handling.

**Runtime class.** Gains an `error-policy` slot with
`development-policy` as the default.

**New condition classes.** `remote-message-error` and its
accessors.

**New classes.** `development-policy`, `service-policy`, and the
`handle-runtime-error` generic function.

**New parameter.** `*default-runtime-error-policy*`.

**Reserved operations.** `:apis/error`, `:apis/ping`,
`:apis/pong`, `:apis/describe`, `:apis/description`.

**No changes to** the message class, the worker class, the
serialization layer, or the addressing scheme.


## Open Questions

- **Should `:apis/error` replies be guaranteed?** If the transport
  itself fails (network partition), the sender may never learn that
  the message wasn't processed. Is silence acceptable, or should
  the transport layer provide a local timeout-based notification?

- **How should `:apis/describe` represent operations?** A flat
  list of `(class-name operation)` pairs is simple but doesn't
  capture the shape of expected data. A richer description
  (expected initargs, types) is more useful for tooling but harder
  to maintain. The right level of detail is not yet clear.

- **Should there be an `:apis/version-check` operation?** A
  lightweight check that just compares Apis library versions,
  without the full capability description. Useful for detecting
  scenario 1 (different Apis versions) without the overhead of
  scenario 2 (interface enumeration).
