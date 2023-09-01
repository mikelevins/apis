Apis: a worker bee for building application hives

Apis is a library that supports shared-nothing concurrency and distributed processing. It provides:

- agent [class]: an autonomous worker. An agent has an event loop that
  runs in its own thread, its own thread-local state, and the ability
  to communicate with other agents by sending and receiving
  messages.

- messenger [class]: a message-delivery service. A messenger provides
  a per-process public port that can receive messages, thread-safe
  message queues, and a mechanism for transporting messages to remote
  agents and delivering them to local ones. The messenger serializes
  each message and then delivers it to the destination messenger. When
  a messenger receives a message it reads it from the communication
  channel and adds it to a receive queue for the receiving program to
  process.

- message [class]: represents a communication between agents. A
  message is a serialized Lisp object. It may contain any data that
  can be serialized by cl-store.

- envelope [class]: Wraps a message with the host address and port of
  its destination. The messenger uses this information to route the
  message to its destination, which may be local or remote.

An Apis program consists of one or more agents in one or more
processes. If there is more than one agent, and if the agents must
coordinate or transfer data, then they do so by sending messages.

An agent may spawn other agents. If spawning one in the same process,
the spawning agent can simply call the apis API to spawn an agent,
obtaining an immediate address as the result value.

If spawning an agent in a different process, whether local or remote,
the spawning agent sends a :spawn message to the appropriate
messenger, which then calls the spawn API on its behalf and sends back
the resulting agent's host and port.

Sending a Message

To send a message, a programmer uses the following steps:

1. Construct a suitable message, containing whatever Lisp values are needed.

2. Serialize the the message and insert it into an envelope, then address the envelope,
   inserting the correct destination host and port.

3. Enqueue the envelope+message for devlivery. From this point, the
   messenger takes over.

4. The messenger delivers the message. If it's immediate, the
   messenger inserts it directly into the proper receive
   queue. Otherwise, it moves to the next step.

5. The messenger packages the envelope+message for delivery. This
   means compiling and serializing the data, encrypting it, and
   converting it to wire format.

6. The messenger writes the data to the destination
   messenger.

Receiving a Message

1. The messenger reads data from its listen socket. It decodes
   the octet vector, decrypts it, and deserializes it to reconstruct
   an envelope+message.

2. The messenger reads the message+envelope data and deserializes it,
   inserting it into its receive queue. The local application then
   retrieves the decoded message and dispatches it to an appropriate
   local handler.

Processing an Incoming Message

An agent continuously runs an event loop that waits for a message to
become available on its receive queue. When a message becomes
available, the agent's worker thread wakes up, dequeues the
envelope+message, and handles it.

"Handling" a message is defined by the receiving
application. Typically it means examining the message data for
discriminating information that enables the application to route it to
an appropriate handler function.

Such handler functions are defined by the application that uses apis.


