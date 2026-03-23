# Next-Available Runtime Architecture

## Summary

This document describes a proposed architecture for Apis in which
workers (actors) have no direct relationship to OS threads. Instead, a
runtime object owns a fixed set of threads and gives each worker a
turn to process one message whenever a thread becomes available. The
result is simpler than the current thread-per-worker design while
supporting far greater concurrency.

## Core Concepts

**Worker.** An identity, a message queue, and a set of
`handle-message` methods. A worker does not own a thread, a
semaphore, or any concurrency primitive. It does not need to be
started or stopped.

**Runtime.** An object that owns a pool of OS threads and a
ready-queue of workers that have pending messages. Each runtime thread
loops: pull the next ready worker, let it process one message, then
either return it to the ready-queue (if it has more messages) or mark
it idle. The runtime is infrastructure; user code does not interact
with it directly under normal circumstances.

**Message.** Unchanged from the current design: an object carrying
an id, sender, recipient, operation keyword, data, timestamp, and
time-to-live.

## Worker States

A worker is always in exactly one of three states:

- **Idle** — the message queue is empty.
- **Ready** — one or more messages are queued; the worker is on the
  runtime's ready-queue waiting for a turn.
- **Running** — a runtime thread is executing one of the worker's
  handlers right now.

Transitions:

    idle → ready      (a message arrives via send)
    ready → running   (the runtime gives the worker a turn)
    running → ready   (handler completes; more messages pending)
    running → idle    (handler completes; queue is empty)

Only the runtime moves a worker from ready to running. This
guarantees that at most one message is being processed per worker at
any time, preserving the actor serialization invariant without
per-worker locks or semaphores.

## How send Works

1. Push the message onto the target worker's queue.
2. If the worker was idle, mark it ready and enqueue it on the
   runtime's ready-queue.

That's all. There is no semaphore to signal, no thread to create.

## How the Runtime Schedules

Each runtime thread runs the same loop:

1. Pull the next worker from the ready-queue (block if empty).
2. Pop one message from the worker's queue.
3. Call `receive`, which dispatches to `handle-message`.
4. If the worker's queue is non-empty, put it back on the
   ready-queue. Otherwise mark it idle.
5. Go to 1.

Because workers get one message per turn and return to the back of
the ready-queue, scheduling is naturally fair. No worker can starve
another.

## Fairness

The ready-queue is FIFO. Each turn processes one message. A worker
that receives a burst of 1000 messages gets 1000 turns, but those
turns are interleaved with turns for every other ready worker. This
is a structural property of the design, not an additional mechanism.

## What the User Sees

The public API is smaller than the current one:

- `make-instance 'worker` — create a worker. No start/stop needed.
- `handle-message` — define behavior via CLOS method specialization,
  exactly as today.
- `send` — send a message, exactly as today.
- `message` — construct a message, exactly as today.

A default global runtime exists, analogous to GCD's global queues.
Most programs never reference it. Programs that need to can create
additional runtimes or tune the default one.

## Path to Distribution

Because workers have no direct relationship to threads, and handlers
have no knowledge of where messages come from, the architecture
extends to distributed messaging without API changes:

- **Inbound:** A transport layer receives a message from the network
  and calls `send` on the local target worker. The runtime treats it
  like any other message.
- **Outbound:** `send` resolves the target. If it is local, enqueue
  as usual. If it is remote, serialize and transmit. The sending
  worker's code is identical in both cases.

The only new user-visible concept for distribution is addressing:
how to name a worker on a remote host. The core API — `send`,
`handle-message`, `message` — does not change.

## Comparison with Current Design

| Aspect                  | Thread-per-worker (current) | Next-available runtime        |
|-------------------------|-----------------------------|-------------------------------|
| Threads                 | One per worker              | Fixed pool, shared            |
| Per-worker primitives   | Semaphore, thread           | None                          |
| Worker lifecycle        | start / stop required       | Not needed                    |
| Max practical workers   | Hundreds (OS thread limits) | Thousands+                    |
| Scheduling fairness     | N/A (dedicated threads)     | Structural (FIFO ready-queue) |
| Actor serialization     | Implicit (one thread)       | Enforced by runtime           |
| Handler constraints     | None                        | Long handlers delay others    |
| Path to distribution    | Requires reconciling models | Natural extension             |
| Implementation size     | ~150 lines                  | Comparable or smaller         |

## Handler Discipline

The one constraint this architecture introduces: message handlers
should be reasonably short. A handler that blocks or runs for a long
time occupies a runtime thread, reducing throughput for other
workers. This is analogous to the discipline required by GCD blocks
or Erlang receive clauses. For the common case of handlers that do a
modest amount of computation and send messages, the constraint is
invisible.
