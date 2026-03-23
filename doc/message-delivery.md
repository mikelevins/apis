# Apis: Message Delivery

## Overview

This document describes the design of message structure,
serialization, and delivery in Apis. The design serves the local
case (messages delivered within a single runtime) while
accommodating future distribution (messages delivered across process
and host boundaries) without structural changes.

The central principles are:

- A message is a self-contained value. It contains no object
  references, pointers, or live-memory dependencies.
- The data payload is restricted to types that can be serialized
  without ambiguity.
- Serialization produces a byte sequence with a clear separation
  between routing envelope and application payload, so that
  infrastructure can inspect routing information without accessing
  payload contents.
- Delivery is a pipeline of composable transforms, so that
  encryption, compression, signing, and other concerns can be added
  without changing the message format or the application API.


## Message Structure

A message has the following fields:

| Field          | Type                     | Purpose                          |
|----------------|--------------------------|----------------------------------|
| **id**         | ULID                     | Unique message identifier.       |
| **from**       | address (ULID or URI)    | Sender's address.                |
| **to**         | address (ULID or URI)    | Recipient's address.             |
| **operation**  | keyword                  | Names the action to perform.     |
| **data**       | plist of permitted types | Application payload.             |
| **timestamp**  | integer (universal time) | When the message was created.    |
| **time-to-live** | integer (seconds)      | Expiration budget.               |
| **cause**      | ULID or nil              | ID of the triggering message,    |
|                |                          | for causal tracing. Optional.    |

The `from` and `to` fields are addresses as defined in
`addressing.md` — either bare ULIDs (local) or full URIs (remote).
They are never object references. This makes the message
serializable as a self-contained value.

The `cause` field is optional. When a handler sends a new message
in response to a received message, it may set `cause` to the
received message's ID. This enables causal tracing (see
`future-extensions.md`) at negligible cost.


## Permitted Payload Types

The `data` field is a plist whose values must be drawn from the
following closed set of types:

- **Integers** — arbitrary precision.
- **Floats** — single-float and double-float.
- **Rationals** — ratios.
- **Strings** — character strings.
- **Symbols** — keywords and interned symbols. The symbol's package
  is included in the serialized form to ensure correct identity
  across processes.
- **Booleans** — `t` and `nil`.
- **Lists** — proper lists whose elements are themselves permitted
  types.
- **Vectors** — one-dimensional arrays whose elements are permitted
  types.
- **Plists and alists** — as compositions of the above.

Notably excluded:

- Hash tables, CLOS instances, structures.
- Functions, closures, generic functions.
- Streams, pathnames, packages.
- Circular structures.

The rule is simple: **the payload must be a tree of the permitted
types, with no cycles and no opaque objects.** If a value cannot be
printed readably and reconstructed by the reader without loss of
identity or meaning, it does not belong in a message.

Validation of payload types is performed at serialization time. In
the local-only case (no serialization), no validation overhead is
incurred. When serialization is active — either for transport or
for tracing — the serializer validates as it walks the structure
and signals a clear condition on encountering a non-permitted type.


## Serialization Format

The initial serialization format is **s-expressions**, for three
reasons:

1. Common Lisp has a built-in printer and reader. Serialization is
   nearly trivial to implement.
2. The serialized form is human-readable, which is invaluable
   during development and debugging.
3. The format is self-describing: no schema negotiation is needed
   between sender and receiver.

A binary format may be introduced later as an optimization. The
serialization interface is defined so that the format is
pluggable: any implementation that converts a message to a byte
sequence and back is acceptable.

### Envelope and Payload Separation

A serialized message consists of two parts:

```
[envelope] [payload]
```

The **envelope** contains the routing and metadata fields: id, from,
to, operation, timestamp, time-to-live, and cause. These are
always serialized in cleartext so that infrastructure — routing
nodes, relays, logging — can inspect them without special access.

The **payload** contains the data field. It is serialized separately
so that transforms (see below) can be applied to the payload
independently of the envelope.

In the s-expression format, the complete serialized message is a
two-element list:

```lisp
((:id "01ARZ3NDEKTSV4RRFFQ69G5FAV"
  :from "apis://host-a.example.com/01ARZ3NDEKTSV4RRFFQ69G5FAV"
  :to "apis://host-b.example.com/01BXQM6K3JANNHPS0BQ0DQZTGV"
  :operation :transfer
  :timestamp 3953040000
  :time-to-live 600
  :cause "01ARZ3MKAT4V2CRRFFQ10A3B7X")
 (:account-number "8675309"
  :amount 1250.00
  :currency :usd))
```

The first element is the envelope. The second is the payload. They
are serialized and deserialized as distinct units.

### Serializer Interface

The serializer exposes three operations:

- **serialize-message** `(message) → (values envelope-bytes payload-bytes)`
  Serializes a message into separate envelope and payload byte
  sequences. Validates the payload; signals a condition if the data
  contains non-permitted types.

- **deserialize-message** `(envelope-bytes payload-bytes) → message`
  Reconstructs a message from its serialized parts.

- **serialize-message-full** `(message) → bytes`
  Convenience function that serializes and concatenates both parts
  with a length prefix so the boundary can be recovered. Used when
  no per-part transforms are needed.


## The Transform Pipeline

Message delivery is a pipeline of composable stages. Each stage
takes bytes in and produces bytes out. The stages are:

```
send
  → serialize (message → envelope bytes + payload bytes)
  → transform (zero or more: encrypt, compress, sign, ...)
  → frame (combine envelope + transformed payload for transport)
  → transport (bytes → network)
```

And symmetrically on the receiving end:

```
transport (network → bytes)
  → deframe (extract envelope + transformed payload)
  → inverse transform (decrypt, decompress, verify, ...)
  → deserialize (envelope bytes + payload bytes → message)
  → local send
```

### Transforms

A transform is a pair of functions:

- **apply** `(bytes) → bytes`
- **reverse** `(bytes) → bytes`

Transforms compose: the output of one is the input of the next.
They are applied to the payload portion of the message. The
envelope remains in cleartext unless a transform is explicitly
applied to the full framed message (for the case where even
metadata must be hidden).

Examples of transforms:

| Transform      | Apply                | Reverse              |
|----------------|----------------------|----------------------|
| Encryption     | encrypt with key     | decrypt with key     |
| Compression    | compress             | decompress           |
| Signing        | append signature     | verify and strip     |
| Base64 encoding| encode               | decode               |

### Transform Policies

Transforms are configured on the **transport**, not on individual
messages. A transport to a given destination carries a policy that
specifies which transforms to apply. For example:

- "All messages to host X are encrypted with key K and compressed."
- "Messages with operation `:payment` are encrypted; others are
  signed only."
- "Local delivery: no transforms."

This keeps security decisions out of application code. The
programmer calls `send`; the transport applies the appropriate
transforms based on the destination and message metadata.

### Why Payload-Envelope Separation Matters

Consider a relay node in a distributed system: host A sends a
message to host C via host B. Host B needs to read the `to` field
to know where to forward the message, but should not need access to
the payload. With envelope-payload separation, B reads the
cleartext envelope for routing and forwards the opaque (possibly
encrypted) payload untouched.

Similarly, a logging system can record message metadata — who sent
what operation to whom, when — without ever seeing the application
data. This is essential for auditability in domains like financial
transactions, where you want comprehensive routing logs without
exposing sensitive payload contents.

### Full-Message Encryption

For the case where even the envelope must be protected — hiding
who is communicating, not just what they are saying — a transform
can be applied to the full framed message (envelope plus payload
together). The outermost transport layer then sees only opaque
bytes plus whatever minimal routing information it needs (e.g., a
destination host address that exists outside the Apis message
envelope entirely, as part of the network transport protocol).


## Local Delivery

In the local case, none of the above machinery is invoked. `send`
resolves the target address in the runtime's local registry,
pushes the message object directly onto the worker's queue, and
marks the worker ready. No serialization, no transforms, no
framing. The pipeline exists but its stages are bypassed entirely.

This is important: the local case pays no cost for the existence
of the distributed delivery path. The transform pipeline is
infrastructure that activates only when a transport is involved.


## Design Decisions and Rationale

**Custom serializer, not a library.** The message format is small
and fixed. The set of permitted types is closed and simple. A
purpose-built serializer for this specific structure is roughly
50–80 lines of code, has no external dependencies, and its behavior
is fully auditable. General-purpose serialization libraries (e.g.,
conspack, cl-store) solve a much harder problem and bring
dependencies and complexity that are unnecessary here.

**S-expressions first, binary later.** Human-readable serialization
is a debugging superpower. The performance cost of text versus
binary is unlikely to matter until message throughput reaches levels
that also demand other optimizations. The serializer interface is
format-agnostic, so a binary implementation can be introduced later
without changing anything above the serialization layer.

**Envelope-payload separation from day one.** Even though the
initial implementation will not encrypt anything, designing the
serialization with this split costs nothing now and avoids a
redesign when encryption is needed. The separation also benefits
logging and tracing immediately, since trace infrastructure can
record envelopes without touching payloads.

**Transforms on the transport, not the message.** Putting security
policy in the transport means application code never makes
encryption decisions. This is both simpler (the programmer doesn't
think about it) and safer (policy is centralized and auditable
rather than scattered across send calls).

**Ironclad for future encryption.** When encryption transforms are
implemented, the recommendation is to use the `ironclad` library
rather than a custom implementation. Cryptography is the one domain
where rolling your own is almost never appropriate. Ironclad is
mature, pure Common Lisp, and covers standard algorithms. This is a
future dependency, not a current one.
