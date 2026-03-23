# Apis: Session Handoff — Stage 2 Complete, Transport Next

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
  addressing.lisp         — URI address parsing and formatting
  runtime.lisp            — runtime, scheduling loop, send, registry
  serialization.lisp      — payload + envelope serialization
tests/
  test-framework.lisp     — minimal test framework (define-test, check, check-equal, check-condition)
  tests.lisp              — core tests (IDs, messages, workers, runtime, send, smoke test)
  serialization-tests.lisp — 25 payload round-trip and error-case tests
  addressing-tests.lisp   — 18 URI parsing/formatting tests
  envelope-tests.lisp     — 18 envelope and message serialization tests
doc/
  addressing.md           — URI address scheme (apis:id, apis://host:port/id)
  message-delivery.md     — message structure, envelope/payload separation, transform pipeline
  serialization.md        — payload types, serializable-data, wire format, 4-stage plan
  api-version-matching.md — error reply protocol, per-runtime error policy, restarts
  next-available-runtime.md — runtime architecture rationale
  initial-implementation-spec.md — spec for current local-only implementation
  future-extensions.md    — serialization, distribution, observability, supervision, priority
  handoff-stage2.md       — previous session handoff (Stage 1 complete)
  handoff-stage3.md       — THIS FILE (Stage 2 complete)
```

All 100 tests pass. Run with:
```lisp
(asdf:load-system :apis/tests :force t)
(apis-tests:run-tests)
```


## What Was Done This Session

### 1. URI addressing (new file: `src/addressing.lisp`)

- Addresses are integers (local ULID) or strings (remote URI)
- `format-address`: integer or string → URI string
  (`"apis:XXXX..."` for local, passthrough for remote)
- `parse-address`: URI string → `(values host port worker-id)`;
  host and port are nil for local addresses
- `resolve-deserialized-address`: local URIs → integer ULIDs,
  remote URIs → preserved as strings
- `malformed-address` condition for invalid input
- Accepted URI forms:
  - `apis:WORKERID` — local
  - `apis://host/WORKERID` — remote, default port
  - `apis://host:port/WORKERID` — remote, explicit port

### 2. Envelope serialization (additions to `src/serialization.lisp`)

- `defstruct (envelope (:type list))` — seven fields: id, from,
  to, operation, timestamp, time-to-live, cause. The struct IS
  the wire form (a plain list), so serialization is print/read.
- `serialize-envelope`: message → envelope list with wire-form
  values (ULID strings, URI strings, keyword, integers)
- `serialize-message` → `(values envelope-string payload-string)`
- `deserialize-message` → reconstructs a message from both parts
- `serialize-message-full` → single string (two-element list:
  envelope-form + payload-wire-form)
- `deserialize-message-full` → reconstructs from single string
- `reconstruct-message-from-envelope`: internal helper that
  converts wire-form fields back to rich values

### 3. Message class updates (`src/message.lisp`)

- `from` and `to` slot types widened to `(or integer string null)`
- `print-address` helper for display (handles nil, integer, string)
- `print-object` updated to use `print-address`

### 4. Send routing (`src/runtime.lisp`)

- Extracted `deliver-locally` helper (takes msg, worker-id, runtime)
- `send` now dispatches on address type via `etypecase`:
  - `null` → dead letter ("no recipient")
  - `integer` → `deliver-locally` (unchanged from before)
  - `string` → `parse-address`, then:
    - local URI (no authority) → `deliver-locally`
    - remote URI → dead letter ("no transport configured")
- The remote dead-letter path is the explicit seam where Stage 3
  plugs in

### 5. Test coverage (100 tests total)

- 18 addressing tests: format, parse (local/remote/port variants),
  round-trips, resolve, 8 malformed-input error cases
- 18 envelope tests: struct, serialize-envelope, print/read
  round-trip, serialize-message/deserialize-message (local,
  remote, nil, complex payloads), full-form round-trips,
  wire structure, consistency between two-part and full paths,
  print-object with new address types
- 4 new send tests: nil-to dead letter, remote dead letter,
  delivery via local URI, unknown local URI dead letter


## Key Design Decisions

All decisions from the Stage 1 handoff still apply, plus:

- **Addresses are integers or strings, not a wrapper type.**
  Integer means local, string means remote URI. `integerp` is
  the routing test. A bare integer is just an abbreviation for
  a local URI. This keeps dispatch trivial (a few `etypecase`
  branches) and avoids a premature abstraction for a type whose
  full requirements won't be clear until the transport exists.

- **No intermediate address representation.** If we need
  `parse-address` on integers later, we make it a generic
  function and add a method on `integer`. CLOS gives us that
  upgrade path for free.

- **The envelope struct IS the wire form.** `defstruct (:type list)`
  means the runtime representation is a plain list. All fields
  carry wire-form values (ULID strings, URI strings, keyword,
  integers). No intermediate "rich envelope" → "wire envelope"
  conversion step. One less thing to remember during testing and
  maintenance.

- **`serialize-message-full` uses a two-element list** as the
  framing format: `(envelope-form payload-wire-form)`. The Lisp
  reader naturally recovers the boundary. This is the convenience
  form; the two-part API (`serialize-message`/`deserialize-message`)
  is the structural prerequisite for selective payload encryption.

- **`deliver-locally` extracted as a named helper** so that `send`
  routing reads as dispatch logic, not delivery mechanics.

- **Remote addresses dead-letter with a clear reason.** The string
  `"No transport configured for remote address ..."` marks the
  exact seam where transport delivery will be inserted.


## Implementation Stages

```
Stage 1: Payload serialization         [COMPLETE]
    ↓
Stage 2: Envelope + addressing         [COMPLETE]
    ↓
  send routing updated                  [COMPLETE]
    ↓
Stage 3: Transport + transform pipeline [NEXT]
    ↓
Stage 4: Encryption transforms
```

## What Comes Next: Stage 3

### The transport layer

Per `doc/message-delivery.md`, the pipeline is:

```
send
  → serialize (message → envelope bytes + payload bytes)
  → transform (zero or more: encrypt, compress, sign, ...)
  → frame (combine envelope + transformed payload for transport)
  → transport (bytes → network)
```

And the reverse on the receiving end.

### Key design questions for Stage 3

1. **Transport interface.** What does a transport object look like?
   It needs: a destination (host:port), a connection, transform
   policies, and send/receive methods. CLOS class with generic
   function protocol is the natural shape.

2. **Framing protocol.** The two-part serialization produces
   envelope bytes and payload bytes. Framing combines them with
   length prefixes so the receiver can recover the boundary. The
   format needs to be specified (e.g., 4-byte big-endian length
   prefix for each part).

3. **Transform pipeline.** A transform is a pair of functions
   (apply/reverse). Transforms compose. They are configured on
   the transport via policies. The initial implementation can
   have zero transforms (passthrough) — the pipeline exists but
   does nothing. Encryption (Stage 4) plugs in as a concrete
   transform later.

4. **Connection management.** TCP connections between runtimes.
   Connection pooling, reconnection, etc. Can start simple
   (one connection per transport) and evolve.

5. **Integration with `send`.** The remote dead-letter path in
   `send` becomes: look up or create a transport for the target
   authority, serialize, and hand off.

6. **`from` auto-population.** Outbound messages need a `from`
   address that includes the local authority, so remote
   recipients can reply. This probably happens at the transport
   layer, just before serialization.

### Implementation approach

Start with the transport interface and framing, test with
loopback (serialize → deserialize without actual network I/O),
then add TCP. The transform pipeline can start as a no-op
passthrough and gain real transforms in Stage 4.


## Documents to Read First

In priority order:
1. `doc/message-delivery.md` — the transform pipeline spec
2. `doc/addressing.md` — URI scheme and resolution rules
3. `doc/serialization.md` — Stage 3 section for transport context
4. `src/runtime.lisp` — current `send` with address dispatch
5. `src/serialization.lisp` — the serialize-message API
6. `src/addressing.lisp` — parse-address and format-address
