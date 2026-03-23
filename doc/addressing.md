# Apis Addressing

## Overview

Every Apis worker has a unique address. Locally, the address is used
to route messages through the runtime's registry. In a distributed
system, the address carries enough information to route messages to
workers in remote processes and on remote hosts. The addressing
scheme is designed so that local usage is simple and lightweight,
while distribution requires no changes to user code — only richer
addresses.

## Worker IDs

Each worker is assigned a ULID (Universally Unique Lexicographically
Sortable Identifier) at creation time. A ULID is a 128-bit value
composed of:

- 48 bits of millisecond-precision timestamp
- 80 bits of cryptographic randomness

This provides several useful properties:

- **Uniqueness without coordination.** Any runtime, on any host, can
  generate IDs independently with negligible collision probability.
- **Time-sortability.** IDs sort chronologically by creation time,
  which is useful for debugging and logging.
- **Compact string form.** The canonical representation is 26
  characters of Crockford base32 (e.g., `01ARZ3NDEKTSV4RRFFQ69G5FAV`),
  which is short enough for log output and REPL interaction.

The ULID replaces the current `makeid` function, which produces
64-bit values with only 32 bits of randomness — sufficient for a
single process but inadequate when multiple independent runtimes
generate IDs concurrently.

## Address Syntax

Apis addresses use URI syntax:

```
apis:<worker-id>
apis://host/worker-id
apis://host:port/worker-id
```

### Local addresses

A local address has no authority component:

```
apis:01ARZ3NDEKTSV4RRFFQ69G5FAV
```

This is the minimal form. It refers to a worker in the local
runtime. The runtime resolves it by looking up the worker ID in its
own registry.

In Common Lisp code, most local usage will not involve explicit URI
strings. The `send` function accepts a worker ID directly, and the
runtime resolves it:

```lisp
(send (message :to worker-id :operation :greet))
```

The URI form exists so that addresses can appear in serialized
messages, log output, configuration, and REPL interaction with a
consistent human-readable format.

### Remote addresses

A remote address includes an authority (host and optional port):

```
apis://somehost.example.com/01ARZ3NDEKTSV4RRFFQ69G5FAV
apis://somehost.example.com:9100/01ARZ3NDEKTSV4RRFFQ69G5FAV
```

When `send` encounters an address with an authority component, it
delegates to the transport layer, which serializes the message and
delivers it to the named host. The receiving host extracts the
worker ID from the path and performs a local `send`.

### Reply addressing

When a worker sends a message to a remote worker, the `from` field
carries the sender's full address including authority, so that the
recipient can reply without knowing in advance where the sender
lives:

```
from: apis://origin-host.example.com/01ARZ3NDEKTSV4RRFFQ69G5FAV
to:   apis://remote-host.example.com/01BXQM6K3JANNHPS0BQ0DQZTGV
```

The handler at the remote end sees these as opaque addresses. A
reply is just another `send`.

## Resolution

Address resolution proceeds in two steps:

1. **Parse.** Extract the authority (if any) and the worker ID.
2. **Route.**
   - If no authority, or the authority names the local host: look up
     the worker ID in the local runtime's registry and deliver
     locally.
   - If the authority names a remote host: hand the message to the
     transport layer for network delivery.

The registry is a simple map from worker ID to worker object,
maintained by the runtime. Workers are added to the registry at
creation and removed when they are no longer reachable.

## Design Notes

**Why URIs?** URIs provide a standard, extensible syntax for naming
resources across network boundaries. The authority/path structure
maps cleanly onto host/worker-id, the scheme identifies the
protocol, and the format is familiar, parseable, and printable.

**Why ULIDs over UUIDs?** ULIDs are time-sortable, which UUIDv4 is
not. ULIDv7 (the time-sortable UUID variant) would also work, but
ULIDs have a more compact string representation (26 characters vs.
36) and do not require hyphens.

**Extensibility.** The URI path can accommodate future structure if
needed. For example, if a host runs multiple named runtimes:

```
apis://host:port/runtime-name/worker-id
```

This is not part of the current design but the syntax accommodates
it without breaking existing addresses.
