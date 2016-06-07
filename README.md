erlang_chat
===========

Introduction
------------
erlang_chat is a simple room-based chat client/server, based on one implemented
in Java earlier.

The goal of the project is to replicate the functionality of the server in
Erlang, to benefit from the reasoning, concurrency support and thread safety
that Erlang essentially includes as standard. It is intended entirely as a
learning project for the author, with intended learning topics including:
  * Microservice architectures
  * Synchronous and asynchronous message passing
  * Network protocol handling
  * Stateful functional programming and I/O handling
  * General Erlang and OTP experience

Client
------

There are currently two clients, one in Erlang and one in Node.js.

### Erlang Client
To run the Erlang client, it must be built first. It comes with an Erlang.mk
file and its own Makefile, so all that is required is to run:

  ~~~~
  [user@host chat\_client] $ make all
  ~~~~

This will download any dependencies if need be, and build the client modules.

The Erlang client works only when run with the following commands from the
`chat_client` directory:

Open the Erlang shell with the required paths:

  ~~~~
  [user@host chat\_client] $ erl -pa ./ebin ./deps/jiffy/ebin
  ~~~~

From the Erlang shell, run the start program:

  ~~~~
  1> chat\_client\_receiver:start\_link(ServerHostname, Port).
  ~~~~

Ideally, the client would execute when `make run` is called, or by executing

  ~~~~
  [user@host chat\_client] $ ./\_rel/chat\_client\_release/bin/chat\_client\_release foreground
  ~~~~

However, there is currently a bug I haven't solved involving the prompt:

  ~~~~
  Input = io:get\_line(PromptString).
  ~~~~

doesn't seem to return under the release conditions.

### Node Client
The Node.js client is a single file under `/chat_client/node/simpleClient.js`.
It supports all the features that the Erlang and original Java clients do, and
is the current one I use for testing. Start it with:

  ~~~~
  [user@host chat\_client/node] $ node simpleClient.js
  ~~~~

You will require a Node.js installation to run this, of course.

Server
------

The server is contained with `/chat_server/` and can be built, much like the
client, with:

  ~~~~
  [user@host chat\_server] $ make all
  ~~~~

This will download any dependencies it needs and then compile the modules.

To run the server, simple run:

  ~~~~
  [user@host chat\_server] $ make run
  ~~~~

Or to run it as a background process:

  ~~~~
  [user@host chat\_server] $ ./\_rel/chat\_server\_release/bin/chat\_server\_release
  ~~~~

Currently the server only supports the single *MainHall* chat room and simple
messaging. Planned additions are to allow name changes, room creation, room
ownership and room bans.

Requirements
------------

To build the Erlang parts of the client or server, you will require:
  * Erlang OTP 17 or higher
  * `make`

To build the Node.js client, you will require:
  * A recent Node.js installation
