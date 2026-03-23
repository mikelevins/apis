# Apis: Session Handoff — Serialization Complete, Addressing Next

## Project Location

`/Users/mikel/repos/lisp/apis`

## What Apis Is

Apis is a Common Lisp actor-style message-passing concurrency
library. Workers (actors) have ULIDs, concurrent message queues,
and behavior defined via CLOS `handle-message` methods
specialized on operation keywords. A runtime owns a fixed thread
pool and a FIFO ready-queue; scheduling threads loop pulling
ready workers and dispatching one message per turn. The design is
local-only today but architected for distribution.

Dependencies: `bordeaux-threads`, `closer-mop`,
`queues.simple-cqueue`. Version 0.9.2.


## Repository Structure

```
apis.asd                  — system definition
version.lisp              — "0.9.2"
src/
  package.lisp            — package with all exports
  parameters.lisp         — global defaults
  id.lisp                 — ULID generation (monotonic single-clock)
  message.lisp            — message class and constructor
  worker.lisp             — worker class, handle-message protocol
  runtime.lisp            — runtime, scheduling loop, send, registry
  serialization.lisp      — payload serialization (Stage 1 complete)
tests/
  test-framework.lisp     — minimal test framework (define-test, check, check-equal, check-condition)
  tests.lisp              — core tests (IDs, messages, workers, runtime, send, smoke test)
  serialization-tests.lisp — 25 round-trip and error-case tests
doc/
  addressing.md           — URI address scheme (apis:id, apis://host:port/id)
  message-delivery.md     — message structure, envelope/payload separation, transform pipeline
  serialization.md        — payload types, serializable-data, wire format, 4-stage plan
  api-version-matching.md — error reply protocol, per-runtime error policy, restarts
  next-available-runtime.md — runtime architecture rationale
  initial-implementation-spec.md — spec for current local-only implementation
  future-extensions.md    — serialization, distribution, observability, supervision, priority
```

All tests pass. Run with:
```lisp
(asdf:load-system :apis/tests)
(apis-tests:run-tests)
```


## What Was Done This Session

### 1. Serialization layer (Stage 1 — complete)

- `serializable-data` abstract CLOS base class with custom
  metaclass `serializable-data-class` via `closer-mop`
- `:serializable t` slot option marks which slots cross the wire
- `serialize-payload` / `deserialize-payload` convert data plists
  to/from s-expression strings
- Wire format uses keyword tags (`:lst`, `:vec`, `:obj`, `:sym`)
  for compound values; primitives are self-representing
- Cycle and shared-structure detection via visited-set (both are
  errors)
- Closed type hierarchy: primitives, collections,
  `serializable-data` subclasses
- 25 tests covering all types, nesting, error cases

### 2. Timestamp monotonicity fix

- `current-time-in-milliseconds` in `id.lisp` was combining two
  unsynchronized clocks (`get-universal-time` +
  `get-internal-real-time`), causing occasional backward
  timestamps
- Fixed: single-clock approach anchored to
  `get-internal-real-time` with a one-time calibration against
  `get-universal-time` at load time

### 3. Design documents

- `doc/serialization.md` — full spec including 4-stage
  implementation plan
- `doc/api-version-matching.md` — error reply protocol
  (`:apis/error`, `:apis/ping`, `:apis/describe`), per-runtime
  error policy (`development-policy` / `service-policy`),
  `handler-bind` with restarts, thread-blocking considerations,
  `from` field auto-population requirement


## Key Design Decisions

- **No separate `define-serializable` macro.** Serialization info
  is in the `defclass` form via `:serializable t` slot options.
  One definition, one source of truth.
- **`standard-object` only.** No defstruct serialization support.
- **Clobber-inspired reconstruction.** Deserialization calls
  `make-instance` with recovered initarg/value pairs. Schema
  evolution handled by `initialize-instance` machinery.
- **Cycles and sharing are errors**, not preserved. Each message
  is self-contained.
- **Per-runtime error policy**, not global. Stored on runtime
  class. Modeled on Hunchentoot's `*catch-errors-p*` but
  generalized to CLOS dispatch.
- **`handler-bind` not `handler-case`** for error policy, to
  preserve restarts for interactive development.
- **Transport auto-populates `from`** on outbound messages (not
  yet implemented; noted in api-version-matching.md).


## Implementation Stages

```
Stage 1: Payload serialization         [COMPLETE]
    ↓
Stage 2: Envelope serialization        [NEXT — with addressing]
    ↓
Stage 3: Transport + transform pipeline
    ↓
Stage 4: Encryption transforms (ironclad)
```

## What Comes Next: Stage 2 + Addressing

Stage 2 has two interleaved pieces of work:

### A. URI address parsing

Per `doc/addressing.md`:
- `apis:WORKERID` — local address (no authority)
- `apis://host:port/WORKERID` — remote address
- Parse function: URI string → (values host port worker-id)
- Resolution in `send`: if no authority or authority is local,
  deliver locally (as today); if remote, delegate to transport
  (future)
- The `from` and `to` fields currently hold bare ULIDs (integers);
  they will need to accept either ULIDs or URI strings

### B. Envelope serialization

Per `doc/serialization.md` Stage 2 and `doc/message-delivery.md`:
- `serialize-message` → `(values envelope-string payload-string)`
- `deserialize-message` reconstructs a message from both parts
- `serialize-message-full` for the convenience single-string case
- Envelope contains: id, from, to, operation, timestamp, ttl, cause
- Payload contains: data (already handled by Stage 1)
- The two-part split is the prerequisite for selective payload
  encryption later

### Key constraint

Envelope serialization must handle URI-form addresses in from/to,
so addressing and envelope serialization should be developed
together.


## Documents to Read First

In priority order:
1. `doc/addressing.md` — the URI scheme and resolution rules
2. `doc/serialization.md` — especially the Stage 2 section
3. `doc/message-delivery.md` — envelope/payload separation and
   the serializer interface spec
4. `src/serialization.lisp` — the existing payload serializer
5. `src/runtime.lisp` — current `send` implementation
