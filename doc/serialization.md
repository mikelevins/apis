# Apis: Serialization

## Overview

This document specifies the serialization layer for Apis messages.
Serialization converts message payloads to and from a portable
external representation, enabling future network transport and
immediate use in tracing and logging.

The design follows two principles:

- **The local case pays nothing.** When messages are delivered within
  a single runtime, no serialization occurs. The payload is a Lisp
  object passed by reference on a local queue. Serialization
  activates only when a transport is involved.

- **The type system is closed and self-describing.** The set of
  permitted payload types is defined at the type level using CLOS
  inheritance. The serializer validates the payload tree against
  this type hierarchy and rejects values that fall outside it.


## Permitted Payload Types

The `data` field of a message is a plist whose values must be drawn
from the following types:

### Primitive types

- **Integers** — arbitrary precision.
- **Floats** — single-float and double-float.
- **Rationals** — ratios.
- **Strings** — character strings.
- **Keywords** — keyword symbols.
- **Symbols** — interned symbols. Serialized with their package name
  to ensure correct identity across processes.
- **Booleans** — `t` and `nil`.

### Collection types

- **Proper lists** — whose elements are themselves permitted types.
- **Vectors** — one-dimensional simple arrays whose elements are
  permitted types.

### User-defined types

- **`serializable-data` subclasses** — instances of CLOS classes
  that inherit from `apis:serializable-data` and mark their
  serializable slots with the `:serializable t` slot option.

The payload must be a tree of the permitted types, with no cycles
and no shared structure. Circular or shared references within a
payload are an error.


## The `serializable-data` Class

`serializable-data` is an abstract CLOS class that serves as the
root of all user-defined serializable payload types. It uses a
custom metaclass, `serializable-data-class`, which supports a
`:serializable` slot option.

### Defining serializable types

A user defines a serializable type by subclassing
`serializable-data` and marking the relevant slots:

```lisp
(defclass person (apis:serializable-data)
  ((name :initarg :name :accessor person-name :serializable t)
   (age :initarg :age :accessor person-age :serializable t)
   (cache :accessor person-cache :initform nil))
  (:metaclass apis:serializable-data-class))
```

In this example, `name` and `age` cross the wire; `cache` does not.

**Note:** Because `serializable-data` uses the
`serializable-data-class` metaclass, subclasses must either inherit
it or declare it explicitly via `(:metaclass
apis:serializable-data-class)`.

### Requirements for serializable slots

A slot marked `:serializable t` must have:

- An `:initarg`, so that the deserializer can pass the value to
  `make-instance`.
- Values restricted to the permitted types (including other
  `serializable-data` instances, enabling nested objects).

A serializable slot without an initarg is a definition error,
signaled at serialization time.

### Why a superclass rather than a registry

Using a CLOS superclass rather than a side-table registry provides
three advantages:

1. **Type checking.** The serializer tests `(typep value
   'serializable-data)` to determine whether a CLOS instance is
   allowed. This is a fast, standard CLOS type check with no
   registry lookup.

2. **Self-documenting.** The inheritance relationship states the
   contract: this class participates in Apis message passing.

3. **Closed enumeration.** The permitted-types hierarchy has a
   finite set of cases: primitives, collections, and
   `serializable-data`. The serializer's type dispatch is a
   `typecase`, not a registry consultation.


## Metaclass and Slot Option

The `:serializable` slot option is implemented via the MOP using
`closer-mop`. The mechanism:

- `serializable-data-class` is a metaclass (subclass of
  `standard-class`) that overrides `direct-slot-definition-class`
  and `effective-slot-definition-class` to return custom slot
  definition classes that support the `:serializable` initarg.

- `compute-effective-slot-definition` propagates the `:serializable`
  flag from direct to effective slot definitions.

- The function `serializable-slots` inspects a finalized class and
  returns information about its serializable slots: their names and
  initargs.

This approach is portable via `closer-mop`, which is a de facto
standard portability layer comparable to `bordeaux-threads`.


## Serialization Format

The initial format is **s-expressions**, for human readability
during development and debugging. The format is designed so that a
binary encoding can replace it later without changing any interface
above the serialization layer.

### Wire form

Every value is converted to a **wire form** — a Lisp s-expression
that can be printed with `*print-readably*` and read back
unambiguously. The wire form uses type tags to distinguish compound
values:

| Lisp value                     | Wire form                                     |
|-------------------------------|-----------------------------------------------|
| `42`                          | `42`                                          |
| `3.14d0`                      | `3.14d0`                                      |
| `1/3`                         | `1/3`                                         |
| `"hello"`                     | `"hello"`                                     |
| `:name`                       | `:name`                                       |
| `t`                           | `t`                                           |
| `nil`                         | `nil`                                         |
| `'my-pkg:foo`                 | `(:sym "MY-PKG" "FOO")`                       |
| `'(1 :a "b")`                 | `(:lst 1 :a "b")`                             |
| `#(1 2 3)`                    | `(:vec 1 2 3)`                                |
| `<person name="Alice" age=34>`| `(:obj "MY-PKG" "PERSON" :name "Alice" :age 34)` |

Primitive self-representing types (integers, floats, rationals,
strings, keywords, `t`, `nil`) pass through unchanged. Lists,
vectors, and objects use keyword tags at the head of the form.

Elements within tagged forms are themselves recursively converted
to wire form.

### Disambiguation

The tags `:lst`, `:vec`, `:obj`, and `:sym` are reserved keywords
in the wire format. Because every compound value is tagged, and
the deserializer expects a tag at the car of every non-atomic wire
form, there is no ambiguity with user data. A user list that
happens to begin with `:lst` is serialized as
`(:lst :lst ...)` — the outer tag is structural, the inner `:lst`
is data.

### String encoding

The wire form is converted to a string via `prin1-to-string` with
`*print-readably*` bound to `t` and `*package*` bound to the
`apis` package (so that the tags print without package prefixes).
The string is converted back via `read-from-string` with
`*package*` bound to `apis`.


## Serialization and Deserialization

### `serialize-payload` `(plist) → string`

Serializes a message data plist to a string. Validates that all
values are permitted types. Detects circular or shared structure
via a visited-set and signals `circular-payload` if detected.

### `deserialize-payload` `(string) → plist`

Reconstructs a message data plist from its serialized string form.
Reconstructs `serializable-data` instances via `make-instance`
with the recovered initargs.

### Conditions

- **`non-serializable-type`** — signaled when the payload tree
  contains a value that is not a permitted type. Carries the
  offending value.

- **`circular-payload`** — signaled when the payload tree contains
  circular or shared object references. Carries the offending
  object.


## Cycle and Sharing Detection

The serializer maintains a hash table of visited object identities
(via `eq`). When it encounters a cons cell, vector, or
`serializable-data` instance, it checks the visited set:

- If the object has been seen before, signal `circular-payload`.
- Otherwise, record it and continue.

This catches both true cycles and shared structure (DAGs). Shared
structure is treated as an error because each message is
self-contained: there is no cross-message identity to preserve,
and the receiving side would get independent copies anyway. If a
user needs to express shared identity, they should use explicit
references (e.g., ULIDs).

Note: the visited-set check applies only to compound values (cons
cells, vectors, CLOS instances), not to atoms. Two slots that hold
the same string `"Alice"` are not sharing — they are equal values.


## Impact on the Core

**No changes to the runtime, scheduling loop, send, or
handle-message.** Serialization is a layer above the message model.
It does not modify the `message` class, the `worker` class, or any
existing function.

**New dependency:** `closer-mop` is added to the system definition
for portable MOP access.

**New source file:** `serialization.lisp` is added to the `src`
module, loaded after `runtime.lisp`.

**New exports:** `serializable-data`, `serializable-data-class`,
`serialize-payload`, `deserialize-payload`, `non-serializable-type`,
`circular-payload`.


## Implementation Stages

The serialization and transport work follows a strict dependency
chain. Each stage builds on the one before it, and each produces
testable, independently useful functionality. Nothing is
implemented speculatively — each stage is completed and tested
before the next begins.

### Stage 1: Payload serialization (complete)

**What it does.** Serializes and deserializes the `data` plist of
a message — the application payload — to and from s-expression
strings. Includes the `serializable-data` base class, the
`:serializable` slot option via `closer-mop`, type validation,
and cycle detection.

**Public API.** `serialize-payload`, `deserialize-payload`,
`serializable-data`, `serializable-data-class`,
`non-serializable-type`, `circular-payload`.

**Status.** Implemented and tested. Round-trip tests cover all
permitted types including nested `serializable-data` instances.

**Independently useful for:** validating payload types during
development, logging message payloads as readable strings,
REPL-based debugging of message contents.

### Stage 2: Envelope serialization

**What it does.** Serializes the message routing metadata — id,
from, to, operation, timestamp, time-to-live, cause — separately
from the payload. Produces two distinct serialized units: envelope
bytes and payload bytes. This is the two-part structure specified
in `message-delivery.md`.

**Depends on.** Stage 1 (the payload serializer).

**Public API.** `serialize-message` producing `(values
envelope-string payload-string)`, `deserialize-message`
reconstructing a message from both parts, and
`serialize-message-full` for the convenience case where no
per-part transforms are needed.

**Why it is a separate stage.** The envelope/payload separation is
the structural prerequisite for payload-only encryption. Without
it, encryption cannot be applied to the payload independently of
the routing metadata. By implementing and testing the two-part
serialization before any transport or encryption work begins, we
ensure the boundary is clean and stable.

**Independently useful for:** structured message logging where
envelopes are recorded but payloads are omitted or redacted,
tracing infrastructure that inspects routing metadata without
touching application data.

**When to implement.** Alongside the addressing work (URI parsing
and send resolution), since the envelope serialization must handle
URI-form addresses in the `from` and `to` fields.

### Stage 3: Transport and the transform pipeline

**What it does.** Network I/O between runtimes. Implements the
composable transform pipeline specified in `message-delivery.md`:
serialize → transform → frame → transport, and the reverse on
the receiving end. Transforms are byte-to-byte stages configured
on the transport via policies.

**Depends on.** Stage 2 (envelope/payload separation). The
transform pipeline operates on the serialized byte sequences
produced by Stage 2. Without the two-part serialization, there
is nothing to selectively transform.

**Includes:** connection management between runtimes, the framing
protocol (combining envelope and transformed payload with length
prefixes so boundaries can be recovered), and the transform
interface (a pair of functions: apply and reverse).

**When to implement.** After addressing (URI resolution in `send`)
is complete and tested.

### Stage 4: Encryption transforms

**What it does.** Concrete transform implementations for
encryption, signing, and optionally compression. Uses the
`ironclad` library for cryptographic operations.

**Depends on.** Stage 3 (the transform pipeline). Encryption is a
transform; it plugs into the pipeline as a byte-to-byte stage.
Without the pipeline, there is nowhere to insert it.

**Two levels of encryption, both using the same mechanism:**

- **Payload encryption.** The transform is applied to the payload
  bytes only. The envelope remains cleartext, so routing
  infrastructure (relays, logging) can inspect routing metadata
  without access to the payload contents. This is the common case
  and the one the envelope/payload separation was designed for.

- **Full-message encryption.** The transform is applied to the
  entire framed message (envelope plus payload together). The
  outermost transport layer sees only opaque bytes. This is for
  cases where even the metadata — who is communicating with whom
  — must be hidden. Minimal routing information (destination
  host address) exists outside the Apis message format entirely,
  at the network transport protocol level.

**Transform policies** are configured on the transport, not on
individual messages. A transport to a given destination carries a
policy specifying which transforms to apply. Examples: "all
messages to host X are encrypted with key K and compressed,"
"messages with operation `:payment` are encrypted, others are
signed only," "local delivery: no transforms." This keeps security
decisions out of application code — the programmer calls `send`
and the transport applies the appropriate transforms.

**New dependency.** `ironclad` (mature, pure Common Lisp,
standard algorithms). Added only at this stage, not before.

**When to implement.** After the transport (Stage 3) is working
with no transforms, so that the baseline transport can be tested
independently of cryptographic concerns.

### Dependency summary

```
Stage 1: Payload serialization        [complete]
    ↓
Stage 2: Envelope serialization        [next, with addressing]
    ↓
Stage 3: Transport + transform pipeline [after addressing]
    ↓
Stage 4: Encryption transforms          [after transport baseline]
```

Each arrow means "depends on and builds upon." No stage can be
skipped. Each stage produces working, tested code that is
independently useful before the next stage begins.


## What Is Deliberately Out of Scope

- **Payload validation on local send.** In the local case, the
  serializer is never called. Type violations in local payloads
  are the programmer's responsibility until a transport is
  configured.

- **Binary format.** A compact binary encoding can replace
  s-expressions later. The `serialize-payload` /
  `deserialize-payload` interface is format-agnostic.

- **Defstruct support.** Only `standard-object` subclasses (via
  `serializable-data`) are supported. Defstruct serialization is
  omitted as unnecessary for the messaging use case.

- **Cross-payload identity.** Each message is independently
  serialized. There is no mechanism for preserving object identity
  across messages (unlike Clobber's transaction-log-wide identity
  tracking).
