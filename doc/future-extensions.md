# Apis: Future Extensions

This document records areas of future development for Apis. Each
section describes a capability that is not yet implemented but that
the architecture is designed to accommodate. Sections follow a
common structure: motivation, approach, impact on the core, and open
questions.


---


## 1. Serialization

### Motivation

Messages must be serializable before they can be transported across
process or network boundaries. Serialization is also a prerequisite
for persistent logging, replay-based debugging, and durable message
queues.

### Approach

Define a canonical external representation for messages. The natural
candidates are:

- **S-expressions.** Native to Common Lisp, human-readable, easy to
  produce and parse. A message is already a small, flat structure
  (id, from, to, operation, data, timestamp, time-to-live), and
  each field is either a ULID string, a keyword, a plist, or an
  integer. S-expression serialization may be nearly trivial.
- **A binary format.** More compact and faster to parse than text.
  Relevant if message throughput becomes a bottleneck over the
  network. Introduces a dependency or a custom codec.

The `data` field (a plist) is the main design challenge: its values
are arbitrary Lisp objects. Either constrain `data` to a set of
serializable types (numbers, strings, keywords, lists of the same)
or provide a protocol for user-defined serialization of custom types.

### Impact on the Core

None. Serialization is a layer on top of `message`. The `message`
class and `send` function do not change. The transport layer (see
Distribution below) calls the serializer; user code does not.

### Open Questions

- Is it worth supporting multiple serialization formats (e.g.,
  s-expressions for debugging, binary for production)?
- What is the right constraint on the `data` field? A closed set of
  types is simpler; an open protocol is more flexible.


---


## 2. Distribution

### Motivation

The actor model is inherently distribution-friendly. Workers that
communicate only via messages have no shared state that would tie
them to a single process or host. Distributed Apis would allow
systems to scale across machines and support fault-tolerant
architectures where workers on different hosts collaborate.

### Approach

Add a transport layer beneath `send`. When `send` resolves a target
address with a remote authority component (see `addressing.md`), it
hands the message to a transport for serialization and network
delivery. The receiving host deserializes and performs a local `send`.

The transport is an internal abstraction. Possible implementations
include raw TCP connections, TLS streams, WebSockets, or a message
broker. The choice is independent of the Apis API.

### Impact on the Core

Minimal. `send` gains a resolution step: check whether the target is
local or remote. If local, proceed as today. If remote, delegate to
the transport. The runtime, the scheduling loop, `handle-message`,
and the worker model are unchanged.

The addressing scheme (see `addressing.md`) already accommodates
remote targets via the authority component of the URI.

### Open Questions

- How do runtimes discover each other? Static configuration,
  multicast, a registry service?
- How are connection failures handled? Retry? Dead letter? Notify
  the sender?
- Is there a need for delivery guarantees (at-most-once,
  at-least-once, exactly-once)?


---


## 3. Observability and Debugging

### Motivation

Systems of actors are notoriously difficult to debug. Conventional
tools — breakpoints, stack inspection — are poorly suited to
concurrent message-passing systems where problems arise from the
interaction of many independent workers over time. Effective
debugging requires the ability to observe message flow across the
entire system.

### Approach

The next-available runtime architecture provides a natural
observation point: all messages pass through `send` (submission) and
the runtime's scheduling loop (dispatch). Instrumenting these two
points captures the complete life of every message.

**Message tracing.** The runtime can emit a structured trace event
at each stage of message delivery:

| Event         | Meaning                                        |
|---------------|------------------------------------------------|
| **sent**      | `send` was called; message submitted.          |
| **enqueued**  | Message placed in target worker's queue;       |
|               | worker marked ready.                           |
| **dispatched**| Runtime gave the worker a turn; handler called.|
| **completed** | Handler returned (success or error).           |
| **dead**      | Message could not be delivered.                |

Each event records the message ID, sender, recipient, operation,
and timestamp. With tracing enabled, the scenario "a worker isn't
seeing messages" reduces to filtering events by worker ID and
observing where the chain breaks: not sent? sent but to the wrong
ID? enqueued but never dispatched? dispatched but handler errored?

**Dead letter queue.** Messages whose target ID does not resolve —
locally or remotely — are filed in a dead letter queue with a
reason. This catches addressing errors directly.

**Causal tracing.** Every message carries an ID and a `from` field.
If handlers that send new messages propagate a reference to the
triggering message (e.g., a `cause` or `in-reply-to` field), the
runtime can reconstruct causal chains: message A caused worker X to
send message B to worker Y. This enables backward reasoning — if Y
didn't receive the expected message, what was supposed to trigger it,
and did that upstream message arrive?

**Trace interface.** The trace data can support several levels of
tooling:

- A log stream (structured text, one line per event) for simple
  filtering with standard tools.
- A query interface for filtering by worker, operation, time range,
  or causal chain.
- An interactive inspector for exploring message flow graphically.

The first is trivial to implement; the others are tools built on the
same underlying trace data.

### Impact on the Core

Small. Tracing hooks are added to `send` and the runtime scheduling
loop — the two points that already handle every message. The hooks
are conditional: if tracing is disabled, cost is a single branch.
User code is unaffected; tracing is a runtime concern.

If causal tracing is desired, the `message` class gains an optional
`cause` field. This is a backward-compatible addition.

### Open Questions

- Should tracing be per-runtime, per-worker, or global?
- What is the right default — tracing off, or tracing on with a
  bounded ring buffer?
- How should trace data be stored for distributed systems where
  events span multiple hosts?


---


## 4. Supervision and Fault Tolerance

### Motivation

In long-running actor systems, individual workers may fail. A
supervision strategy defines what happens when a handler signals an
error: restart the worker, discard the message, escalate to a
parent, or shut down a group of related workers.

### Approach

*To be determined.* Erlang's supervisor trees are the well-known
model. The key question for Apis is how much of that model is
needed and what fits naturally with the CLOS-based handler dispatch.

### Impact on the Core

*To be assessed.* Supervision likely requires the runtime to catch
handler errors (it already does, implicitly, since the scheduling
loop calls handlers) and consult a policy object. The worker model
may need a notion of parent/child relationships.

### Open Questions

- Is a full supervisor tree needed, or is a simpler per-worker
  restart policy sufficient for most use cases?
- How does supervision interact with distribution? If a remote
  worker fails, who is notified?


---


## 5. Priority Scheduling

### Motivation

Some applications need certain messages or workers to be processed
with greater urgency than others. A financial trading system, a
real-time control application, or any system with mixed
latency-sensitive and background workloads may need finer control
over which work happens first.

Priority has two independent dimensions:

- **Message priority.** Within a single worker's queue, some
  messages are more urgent than others.
- **Worker priority.** Across workers, some workers should receive
  turns before others when the runtime has a choice.

These are complementary and can be adopted independently.

### Approach

**Message priority.** Add an optional `priority` field to the
`message` class (default: a neutral value, e.g., 0). Replace each
worker's FIFO message queue with a priority queue. When the runtime
gives a worker a turn, the worker pops its highest-priority message
rather than the oldest. The runtime's scheduling loop does not
change; it still pulls a worker and says "process your next
message." The worker's notion of "next" becomes priority-ordered.

This is a small, localized change: one new message field, one
different queue type per worker. No impact on the runtime or the
public API beyond the new keyword argument to `message`.

**Worker priority.** Add an optional `priority` slot to the
`worker` class (default: a neutral value). Replace the runtime's
FIFO ready-queue with a priority queue. When multiple workers are
ready, higher-priority workers get turns first.

This is a more consequential change because it replaces the
structural fairness guarantee of the FIFO ready-queue. With
priority scheduling, a sustained flood of messages to
high-priority workers can starve low-priority ones indefinitely.

Mitigation strategies:

- **Aging.** A worker's effective priority increases the longer it
  waits in the ready-queue without getting a turn. This bounds
  worst-case starvation: even the lowest-priority worker
  eventually rises high enough to run. The cost is a small amount
  of bookkeeping in the scheduling loop.
- **Budget-based scheduling.** Each priority level is guaranteed a
  minimum share of turns per unit time. High-priority workers get
  more turns, but low-priority workers are never fully starved.
  More complex to implement but gives stronger guarantees.
- **Separate runtimes.** Rather than mixing priorities in a single
  ready-queue, create separate runtimes with different thread
  counts — e.g., 6 threads for high-priority workers, 2 for
  background. Isolation is complete; no starvation is possible.
  This is the coarsest mechanism but the simplest to implement
  and reason about.

**Recommended approach for Apis:** offer priority as an optional
runtime configuration, not as a change to the default behavior.
The default runtime uses a FIFO ready-queue with structural
fairness. Programs that need priority opt in:

```lisp
;; Default: FIFO, fair, nothing to think about
(make-runtime :thread-count 8)

;; Priority-aware: trades fairness for urgency control
(make-runtime :thread-count 8 :scheduling :priority)
```

Or, more simply, as a distinct runtime class so that each
implementation stays small and independent:

```lisp
;; The default class
(make-instance 'runtime :thread-count 8)

;; A priority-aware subclass
(make-instance 'priority-runtime :thread-count 8)
```

Message priority is largely independent of this choice. Even a
FIFO-scheduled runtime benefits from per-worker priority queues,
because individual workers still process their most important
messages first.

### Impact on the Core

**Message priority:** one optional slot on `message`, one change to
the per-worker queue type. No change to the runtime, `send`, or
`handle-message`.

**Worker priority:** the ready-queue changes from a FIFO to a
priority queue. The scheduling loop gains a small amount of logic
(aging or budget tracking, if used). `send` does not change.
`handle-message` does not change.

If implemented as a separate runtime class, the default runtime is
entirely unaffected.

### Open Questions

- Is a numeric priority sufficient, or would named priority bands
  (e.g., `:critical`, `:normal`, `:background`) be more usable?
- Should message priority and worker priority compose? If a
  high-priority message is sent to a low-priority worker, does the
  message's urgency elevate the worker?
- Is aging the right starvation mitigation, or is the separate-
  runtimes approach simpler and sufficient in practice?
- Should priority be settable after creation, or fixed at worker/
  message construction time?


---


## Template for New Sections

When adding a new extension to this document, use the following
structure:

```
## N. Title

### Motivation

Why is this capability needed? What problem does it solve?

### Approach

How might it be implemented? What are the main design options?

### Impact on the Core

What, if anything, changes in the runtime, the worker model, the
message model, or the public API?

### Open Questions

What remains to be decided?
```
