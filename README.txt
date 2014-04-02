Apis: a worker bee for building application hives

Apis is a library that supports shared-nothing concurrency and distributed processing. It provides:

- agent [class]: an autonomous worker. An agent has an event loop that
  runs in its own thread and selects a handler for each incoming
  message, its own thread-local state, and the ability to communicate
  with other agents using messages.

- agent-address [class]: represents an agent for the purposes of
  message delivery. There are three subtypes of agent-address:
  - immediate-agent-address: the other agent is in the same process
  - local-agent-address: the other agent is in another process on the same host
  - remote-agent-address the other agent is on a remote host

  Message-delivery takes the destination agent's location into
  account. Delivery to and immediate address is a simple matter of
  adding a packaged message value to a queue. Delivery to a local
  address means transporting it over a unix-domain socket. Delivery to
  a remote address means transporting it over a UDP network socket.

- messenger [class]: a message-delivery service. A
  messenger provides a per-process public port that can receive
  messages, thread-safe message queues, and a mechanism transporting
  messages to remote agents and delivering them to local ones.

- message [class]: represents a communication between agents in the
  form of an operation identifier plus parameter data.

- envelope [class]: Represents routing and authorization information
  associated with a message in transit. THe routing information
  includes whether the message can safely be dropped or must be resnet
  if it's never delivered, whether it requires an acknowledgement or
  reply, whether it's part of a sequence, and, if so, its index in the
  sequence.

- payload [type]: represents the wire format of a message in its
  envelope as an octet vector

An Apis program consists of one or more agents in one or more
processes. If there is more than one agent, and if the agents must
coordinate or transfer data, then they do so by sending messages.

An agent may spawn other agents. If spawning one in the same process,
the spawning agent can simply call the apis API to spawn an agent,
obtaining an immediate address as the result value.

If spawning an agent in a local or remote process, the spawning agent
sends a :spawn message to the remote messenger, which then calls the
spawn API on its behalf and sends back the resuting agent-address.

The Messaging Language

A message consists of an operation plus a sequence of parameter
values. Together, these values represent a procedure call. 

The source form of a message represents it as an instance of class
message, but the wire format of a message that is in transit is an
octet vector. In order to conserve bandwidth, apis compiles outgoing
messages into a compact form, converting it to compact bytecode for
transport. On the receiving end, the messenger decompiles the wire
format to reconstruct the source form of the sent message.

The messaging language consists of a defined set of operations that
agents can perform. Each operation is defined as a procedure that
accepts some input parameters. The input parameters are values
supplied by the sender of the message, plus the envelope+message used
to transport the message, so that the agent can respond to errors and
exceptional conditions by sending messages back to the originator.

The messaging language defines operations that include:

- requesting version information
- authenticating and authorizing an agent
- agent operations: creation, destruction, migration, update, broadcasting, establishing handlers
- object and state operations: creation, destruction, update, transfer
- store operations: inserting, removing, updating, searching and retrieving
- reflection: querying and updating data about data and about the system itself

Sending a Message

To send a message, a programmer uses the following steps:

1. Construct a suitable message, selecting the proper operation name
   (a keyword) and supplying the required parameter values (arbitrary
   Lisp values).

2. Place the message in an envelope and address the envelope,
   inserting all the information needed to correctly deliver the message.

3. Enqueue the envelope+message for devlivery. From this point, the
   messenger takes over.

4. The messenger checks the RSVP status of the message. If the message
   requires an acknowledgement then it is posted to the messenger's
   ack table. If it requires a reply, then it's posted to the
   messenger's reply table. 

4. The messenger delivers the message. If it's immediate, the
   messenger inserts it directly into the proper receive
   queue. Otherwise, it moves to the next step.

5. The messenger packages the envelope+message for delivery. This
   means compiling and serializing the data, encrypting it, and
   converting it to wire format.

6. The messenger writes a datagram packet to the destination
   messenger.

Receiving a Message

1. The messenger reads a datagram from its listen socket. It decodes
   the octet vector, decrypts it, and deserializes it to reconstruct
   an envelope+message.

2. The messenger reads the envelope. It checks its ack and reply
   tables for any matches. If it finds a match, it removes it from the
   table and inserts it into the envelope's in-acknowledgement-of or
   in-reply-to slot. It also writes the sender's host and port info
   into the envelope's return-address slots. Finally, it inserts the
   envelope+message into the proper destination agent's receive queue
   and notifies the agent of the insertion.

Processing an Incoming Message

An agent continuously runs an event loop that waits for a message to
become available on its receive queue. When a message becomes
available, the agent's worker thread wakes up, dequeues the
envelope+message, and handles it.

"Handling" a message means first reading the envelope, checking for
prerequisites. For example, most messages must be authenticated before
they can be processed. An agent handling such a message first checks
for an auth token, and then, having found one, checks the token for
validity. If the required token is invalid or missing, the agent sends
an error message back to the originator of the incoming message. 

Once the agent decides that a message is authorized, it next unpacks
the message from the envelope and examines its operation-name. Once
again, the agent checks to ensure tha the operation is
authorized. Assuming it is, it then unpacks the parameter data from
the message and calls the operation API with the parameter data as
input arguments.

Agent operations also take the envelope+message as inputs, so that
defined handlers can respond to error conditions by sending messages
to the originator.


